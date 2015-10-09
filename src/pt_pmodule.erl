%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%%     Parametrized module
%%%     this module is used by internal logger in "log_server"
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pt_pmodule).

-export([parse_transform/2, init/2, dispose/1, is_inited/1, get_param/1, get_params/1, format_error/1]).

-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_lib/include/pt_error_macro.hrl").
-include_lib("pt_lib/include/pt_patrol.hrl").


parse_transform(AST, Options) ->
    Module = pt_lib:get_module_name(AST),

    case pt_lib:get_attribute_value(module_parameters, AST) of
        [] -> throw(?mk_parse_error(0, {no_params, Module}));
        _ -> ok
    end,

    Params = pt_lib:get_attribute_value(module_parameters, AST),

    GetParamCalls = pt_lib:match(AST, ast_pattern("pt_pmodule:get_param($_).")),

    lists:foreach(
        fun
            (ast_pattern("$_($P).", Line)) when pt_lib:is_atom(P) ->
                case lists:member(erl_syntax:concrete(P), Params) of
                    true -> ok;
                    false -> throw(?mk_parse_error(Line, {param_not_found, P}))
                end;
            (ast_pattern("$_($P).", Line)) ->
                throw(?mk_parse_error(Line, {param_bad_type, P}))
        end,
        GetParamCalls),

    AST2 = pt_lib:add_function(AST, ast("'__is_inited_PT_PMODULE_FUNCTION'() -> false.", 0)),

    CurParams = lists:duplicate(length(Params), undefined),
    AST3 = pt_lib:add_function(AST2, ast("'__get_params_PT_PMODULE_FUNCTION'() -> @CurParams.", 0)),

    AST4 = pt_recompilable:parse_transform(AST3, Options),
    AST4.

get_param(_) ->
    throw({error, pmodule_not_inited}).

init(Module, ParamValues) ->
    case is_inited(Module) of
        true -> throw({error, already_inited});
        false -> ok
    end,
    pt_recompilable:recompile_orig_module(Module,
        fun (AST, _) ->
            Params = pt_lib:get_attribute_value(module_parameters, AST),
            Dict = lists:zip(Params, ParamValues),
            pt_lib:replace(AST, [
                {
                    ast_pattern("pt_pmodule:get_param($P).", Line), pt_lib:is_atom(P),
                    begin
                        Pc = erl_syntax:concrete(P),
                        PValue =
                            case lists:keyfind(Pc, 1, Dict) of
                                {_, PV} -> PV;
                                false -> throw({error, {param_not_found, Pc}})
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
                    ast_pattern("'__is_inited_PT_PMODULE_FUNCTION'() -> false.", Line),
                    ast("'__is_inited_PT_PMODULE_FUNCTION'() -> true.", Line)
                },
                {
                    ast_pattern("'__get_params_PT_PMODULE_FUNCTION'() -> ...$_... .", Line),
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
                            ParamValues),
                        ast("'__get_params_PT_PMODULE_FUNCTION'() -> $APList.", Line)
                    end
                }
                ])
        end).

dispose(Module) ->
    pt_recompilable:recompile_orig_module(Module,
        fun (AST, _) ->
            pt_lib:replace(AST, [
                    {
                        ast_pattern("'__is_inited_PT_PMODULE_FUNCTION'() -> true.", Line),
                        ast("'__is_inited_PT_PMODULE_FUNCTION'() -> false.", Line)
                    },
                    {
                        ast_pattern("'__get_params_PT_PMODULE_FUNCTION'() -> ...$_... .", Line),
                        begin
                            Params = pt_lib:get_attribute_value(module_parameters, AST),
                            CurParams = lists:duplicate(length(Params), undefined),
                            ast("'__get_params_PT_PMODULE_FUNCTION'() -> @CurParams.", Line)
                        end
                    }
                    ])
        end).

is_inited(Module) ->
    try
        Module:'__is_inited_PT_PMODULE_FUNCTION'()
    catch
        _:_  -> throw({error, not_parameterized})
    end.

get_params(Module) ->
    try
        Module:'__get_params_PT_PMODULE_FUNCTION'()
    catch
        _:_  -> throw({error, not_parameterized})
    end.

format_error({param_not_found, P}) ->
    io_lib:format("Parameter ~p is not found in -module_parameters", [P]);
format_error({param_bad_type, P}) ->
    io_lib:format("Bad type of parameter: ~p, should be atom", [P]);
format_error({no_init, Module}) ->
    io_lib:format("Module ~p is a parameterized module, but pt_pmodule:init/1 is never called", [Module]);
format_error({no_terminate, Module}) ->
    io_lib:format("Module ~p is a parameterized module, but pt_pmodule:terminate/0 is never called", [Module]);
format_error({no_params, Module}) ->
    io_lib:format("Module ~p is a parameterized module, but params list is null (attribute -module_parameters().)", [Module]);
format_error(Unknown) ->
    io_lib:format("Unkwnown error: ~p", [Unknown]).