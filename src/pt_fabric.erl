%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% Parametrized module fabric.
%%% Usage:
%%%
%%%
%%%
%%% -module(my_fabric).
%%%
%%% -include_lib("pt_scripts/include/pt_fabric.hrl").
%%%
%%% -module_parameters([{param1, value1}, {param2, value2} ...]). % Module parameters enumeration (with default values)
%%%
%%% my_function(...) ->
%%%   ...
%%%   ?FABRIC_PARAM(param_name) % module parameters usage
%%%   ...
%%%   ?FABRIC_MODULE_NAME % use it instead of ?MODULE
%%%
%%%
%%%
%%%
%%% -module(using_fabric).
%%%
%%%  my_func() ->
%%%    ....
%%%    my_fabric:build_relative(new_fabric, [{param1, Value1}...]), % generate, compile and load new module called new_fabric with new parameters
%%%    ....
%%%    my_fabric:rebuild([{param2, NewValue1}]), % rebuild my_fabric with new parameters
%%%    ....
%%%
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pt_fabric).

-export([parse_transform/2, build_relative/3, rebuild/2, get_param/1, format_error/1]).

-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_lib/include/pt_error_macro.hrl").
-include_lib("pt_lib/include/pt_patrol.hrl").
-include_lib("pt_lib/include/pt_types.hrl").


parse_transform(AST, _Options) ->
    Module = pt_lib:get_module_name(AST),

    case pt_lib:get_attribute_value(module_parameters, AST) of
        [] -> throw(?mk_parse_error(0, {no_params, Module}));
        _ -> ok
    end,

    Params = pt_lib:get_attribute_value(module_parameters, AST),


    AST1 = pt_lib:add_function(AST,
            ast("build_relative(ModuleName, Params) ->"
                "    pt_fabric:build_relative(@Module, ModuleName, Params).", 0)),

    AST2 = pt_lib:add_function(AST1,
            ast("rebuild(Params) ->"
                "    pt_fabric:rebuild(@Module, Params).", 0)),

    AST3 = pt_lib:add_function(AST2,
            ast("get_fabric_options() -> erlang:error(not_initialized).", 0)),

    AST4 = pt_recompilable:parse_transform(AST3, []),

    AST5 = init(AST4, Module, Params),

    AST5.

get_param(_) ->
    throw({error, pmodule_not_inited}).

build_relative(Module, NewModuleName, Params) ->
    CurParams = Module:get_fabric_options(),
    NewParams =
        lists:foldl(
            fun ({K, V}, Acc) ->
                lists:keyreplace(K, 1, Acc, {K, V})
            end, CurParams, Params),

    try NewModuleName:get_fabric_options() of
        NewParams -> ok;
        ExistParams -> erlang:error({already_exist, ExistParams})
    catch
        error:undef ->
            AST = Module:get_orig_ast(),
            AST1 = pt_recompilable:parse_transform(AST, []),
            AST2 = init(AST1, NewModuleName, NewParams),
            pt_lib:compile_and_load(AST2)
    end.

rebuild(Module, Params) ->

    try Module:get_fabric_options() of
        CurParams ->

            NewParams =
                lists:foldl(
                    fun ({K, V}, Acc) ->
                        lists:keyreplace(K, 1, Acc, {K, V})
                    end, CurParams, Params),

            case NewParams of
                CurParams -> ok;
                _ ->
                    pt_recompilable:recompile_orig_module(Module,
                        fun (AST, _) ->
                            init(AST, Module, NewParams)
                        end)
            end
    catch
        error:undef ->
            erlang:error(not_exist)
    end.

-spec init(AST :: ast(), ModuleName :: module(), [term()]) -> ast().
init(AST, ModuleName, Params) ->

    AST2 = pt_lib:set_module_name(AST, ModuleName),

    pt_lib:replace(AST2, [
                {
                    ast_pattern("pt_fabric:get_param($P).", Line), pt_lib:is_atom(P),
                    begin
                        Pc = erl_syntax:concrete(P),
                        PValue =
                            case lists:keyfind(Pc, 1, Params) of
                                {_, PV} -> PV;
                                false -> throw(?mk_parse_error(Line, {param_not_found, Pc}))
                            end,
                        PValueAst =
                            try
                                pt_lib:abstract(PValue, Line)
                            catch
                                _:_ ->
                                    BPV = erlang:term_to_binary(PValue),
                                    ast("erlang:binary_to_term(@BPV).", Line)
                            end,
                        ast("$PValueAst.", Line)
                    end
                },
                {
                    ast_pattern("pt_fabric:get_module_name().", Line),
                    ast("@ModuleName.", Line)
                },
                {
                    ast_pattern("rebuild(Params) -> ...$_... .", Line),
                    ast("rebuild(Params) ->"
                        "    pt_fabric:rebuild(@ModuleName, Params).", Line)
                },
                {
                    ast_pattern("build_relative(ModuleName, Params) -> ...$_... .", Line),
                    ast("build_relative(ModuleName, Params) ->"
                        "    pt_fabric:build_relative(@ModuleName, ModuleName, Params).", Line)
                },
                {
                    ast_pattern("get_fabric_options() -> ...$_... .", Line),
                    begin
                        APList = lists:foldr(
                            fun (Pd, Acc) ->
                                PdAst =
                                    try
                                        pt_lib:abstract(Pd, Line)
                                    catch
                                        _:_ ->
                                            BPV2 = erlang:term_to_binary(Pd),
                                            ast("erlang:binary_to_term(@BPV2).", Line)
                                    end,
                                ast("[$PdAst | $Acc].", Line)
                            end,
                            ast("[].", Line),
                            Params),
                        ast("get_fabric_options() -> $APList.", Line)
                    end
                }
                ]).

format_error({param_not_found, P}) ->
    io_lib:format("Parameter ~p is not found in -module_parameters", [P]);
format_error({param_bad_type, P}) ->
    io_lib:format("Bad type of parameter: ~p, should be atom", [P]);
format_error({no_params, Module}) ->
    io_lib:format("Module ~p is a parameterized module, but params list is null (attribute -module_parameters().)", [Module]);
format_error(Unknown) ->
    io_lib:format("Unkwnown error: ~p", [Unknown]).