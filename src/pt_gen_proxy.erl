%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Konstantin '$ky' Sivakov, Anton N Ryabkov
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%% Created : 31 May 2010
%%% Changed: 29 Apr 2013
%%% Moved from repository pt_patterms
%%%-------------------------------------------------------------------
-module(pt_gen_proxy).

-include_lib("pt_lib/include/pt_error_macro.hrl").
-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_lib/include/pt_patrol.hrl").
-include("pt_recompilable.hrl").


-patrol([{tty, error}]).

-export([
         parse_transform/2,
         format_error/1
        ]).

%%====================================================================
%% API
%%====================================================================

-define(PT_GEN_PROXY, 'gen_proxy').

-define(PT_GP_PARAM_MODULE, 'module').
-define(PT_GP_PARAM_MODE, 'mode').
-define(PT_GP_PARAM_PROXY, 'proxy').
-define(PT_GP_PARAM_EXTRA_ARGS, 'extra_args').
-define(PT_GP_PARAM_BUILD_EXTRA_ONLY, 'extra_only').
-define(PT_GP_PARAM_BUILD_EXTRA_PREFIX, 'extra_prefix').
-define(PT_GP_PARAM_BUILD_EXTRA_POSTFIX, 'extra_postfix').

-define(PT_GP_PARAM_IGNORE, 'ignore').
-define(PT_GP_PARAM_IGNORE_DEFAULT, [{module_info, 0}, {module_info, 1}, {get_log_tags, 0}]).

-define(PT_GP_MODE_FUN, 'fun').
-define(PT_GP_MODE_MFA, 'mfa').

-define(PT_GP_DEF_PROXY, 'proxy').

-define(PT_GP_ERR_NO_CFG, 'ptprx-err-no-cfg').
-define(PT_GP_ERR_INV_CFG(Cfg), {'ptprx-err-inval-cfg', Cfg}).
-define(PT_GP_ERR_INV_MODE(Mode), {'ptprx-err-inval-mode', Mode}).
-define(PT_GP_ERR_INV_MODULE(Module), {'ptprx-err-inval-module', Module}).
-define(PT_GP_ERR_INV_PROXY(Proxy), {'ptprx-err-inval-proxy', Proxy}).
-define(PT_GP_ERR_INV_EXTRA_ARGS(Args), {'ptprx-err-inval-extra-args', Args}).
-define(PT_GP_ERR_INV_BUILD_EXTRA_ONLY(Flag), {'ptprx-err-inval-build-extra-only', Flag}).
-define(PT_GP_ERR_INV_BUILD_EXTRA_PREFIX(Prefix), {'ptprx-err-inval-build-extra-prefix', Prefix}).
-define(PT_GP_ERR_INV_BUILD_EXTRA_POSTFIX(Postfix), {'ptprx-err-inval-build-extra-postfix', Postfix}).

-define(PT_GP_ERR_INV_IGNORE(Ignore), {'ptprx-err-inval-ignore', Ignore}).

-define(PT_GP_ERR_INV_PROXY_ARR(Proxy, Arr), {'ptprx-err-inval-proxy-arr', Proxy, Arr}).

-define(PT_GP_ERR_LOAD_MODULE(Module, What), {'ptprx-err-load-module', Module, What}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


parse_transform(AST, _Options) ->
    {Module, Mode, Proxy, Extra, Prefix, Postfix, Flag, List} =
        case pt_lib:get_attribute_value(?PT_GEN_PROXY, AST) of
            [] ->
                throw(?mk_parse_error(0, ?PT_GP_ERR_NO_CFG));

            P ->
                makeParams(AST, P)
        end,
    buildProxy(AST, Mode, {Module, Proxy, Extra, Prefix, Postfix, Flag}, List).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-spec format_error(term()) -> string().
%%--------------------------------------------------------------------
%% @doc
%% Format error message, that can be occured during code transformation by pt_get_proxy.
%% @end
%%--------------------------------------------------------------------
format_error(?PT_GP_ERR_NO_CFG) ->
    io_lib:format("There is no gen_proxy config attribute: -~w", [?PT_GEN_PROXY]);

format_error({'ptprx-err-inval-cfg', Cfg}) ->
    io_lib:format("Invalid gen_proxy config param: ~p", [Cfg]);

format_error({'ptprx-err-inval-mode', Mode}) ->
    io_lib:format("Invalid gen_proxy mode : ~p. Should be ~w or ~w", [Mode, ?PT_GP_MODE_FUN, ?PT_GP_MODE_MFA]);

format_error({'ptprx-err-inval-module', Module}) ->
    io_lib:format("Invalid gen_proxy proxy module name: ~p. Should be atom", [Module]);

format_error({'ptprx-err-inval-proxy', Proxy}) ->
    io_lib:format("Invalid gen_proxy proxy function name: ~p. Should be atom", [Proxy]);

format_error({'ptprx-err-inval-f-proxy', Proxy}) ->
    io_lib:format("Invalid gen_proxy fun proxy function: ~w. Should be ~w/1", [Proxy, Proxy]);

format_error({'ptprx-err-inval-m-proxy', Proxy}) ->
    io_lib:format("Invalid gen_proxy mfa proxy function: ~w. Should be ~w/3", [Proxy, Proxy]);

format_error({'ptprx-err-load-module', Module, What}) ->
    io_lib:format("Can't load gen_proxy proxy module ~w. ~p", [Module, What]);

format_error(Unknown) ->
    io_lib:format("Unknown error: ~p", [Unknown]).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%%%===================================================================
%%% Internal functions
%%%===================================================================


makeParams(AST, P) when is_list(P) ->
    P0 =
        case lists:keyfind(?PT_GP_PARAM_PROXY, 1, P) of
            {?PT_GP_PARAM_PROXY, _} ->
                P;

            false ->
                [{?PT_GP_PARAM_PROXY, ?PT_GP_DEF_PROXY} | P]
        end,

    P1 =
        case lists:keyfind(?PT_GP_PARAM_MODE, 1, P0) of
            {?PT_GP_PARAM_MODE, _} ->
                P0;

            false ->
                [{?PT_GP_PARAM_MODE, ?PT_GP_MODE_FUN} | P0]
        end,

    P2 =
        case lists:keyfind(?PT_GP_PARAM_EXTRA_ARGS, 1, P1) of
            {?PT_GP_PARAM_EXTRA_ARGS, _} ->
                P1;

            false ->
                [{?PT_GP_PARAM_EXTRA_ARGS, []} | P1]
        end,

    P3 =
        case lists:keyfind(?PT_GP_PARAM_BUILD_EXTRA_ONLY, 1, P2) of
            {?PT_GP_PARAM_BUILD_EXTRA_ONLY, _} ->
                P2;

            false ->
                [{?PT_GP_PARAM_BUILD_EXTRA_ONLY, true} | P2]
        end,

    P4 =
        case lists:keyfind(?PT_GP_PARAM_BUILD_EXTRA_PREFIX, 1, P3) of
            {?PT_GP_PARAM_BUILD_EXTRA_PREFIX, _} ->
                P3;

            false ->
                [{?PT_GP_PARAM_BUILD_EXTRA_PREFIX, ''} | P3]
        end,

    P5 =
        case lists:keyfind(?PT_GP_PARAM_BUILD_EXTRA_POSTFIX, 1, P4) of
            {?PT_GP_PARAM_BUILD_EXTRA_POSTFIX, _} ->
                P4;

            false ->
                [{?PT_GP_PARAM_BUILD_EXTRA_POSTFIX, ''} | P4]
        end,

    P6 =
        case lists:keyfind(?PT_GP_PARAM_IGNORE, 1, P5) of
            {?PT_GP_PARAM_IGNORE, _} ->
                P5;

            false ->
                [{?PT_GP_PARAM_IGNORE, ?PT_GP_PARAM_IGNORE_DEFAULT} | P5]
        end,


    ok = checkParams(P6),


    {Module, Mode, Proxy, Extra, Prefix, Postfix, Flag, Ignore} =
        parseParams(P6),

    ok =
        checkProxy(AST, Mode, Proxy, length(Extra), Flag),

    ProxyList =
        readModule(Module, Ignore),

    {Module, Mode, Proxy, Extra, Prefix, Postfix, Flag,  ProxyList};

makeParams(_, Other) ->
    throw(?mk_parse_error(0, ?PT_GP_ERR_INV_CFG(Other))).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


checkParams([]) ->
    ok;

checkParams([H | T]) ->
    ok = checkParam(H),
    checkParams(T).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


checkParam({?PT_GP_PARAM_MODULE, Module}) when is_atom(Module) ->
    ok;

checkParam({?PT_GP_PARAM_MODULE, Module}) ->
    throw(?mk_parse_error(0, ?PT_GP_ERR_INV_MODULE(Module)));

checkParam({?PT_GP_PARAM_MODE, ?PT_GP_MODE_FUN}) ->
    ok;

checkParam({?PT_GP_PARAM_MODE, ?PT_GP_MODE_MFA}) ->
    ok;

checkParam({?PT_GP_PARAM_MODE, Mode}) ->
    throw(?mk_parse_error(0, ?PT_GP_ERR_INV_MODE(Mode)));

checkParam({?PT_GP_PARAM_PROXY, Proxy}) when is_atom(Proxy) ->
    ok;

checkParam({?PT_GP_PARAM_PROXY, Proxy}) ->
    throw(?mk_parse_error(0, ?PT_GP_ERR_INV_PROXY(Proxy)));

checkParam({?PT_GP_PARAM_EXTRA_ARGS, Args}) when is_list(Args) ->
    ok;

checkParam({?PT_GP_PARAM_EXTRA_ARGS, Args}) ->
    throw(?mk_parse_error(0, ?PT_GP_ERR_INV_EXTRA_ARGS(Args)));

checkParam({?PT_GP_PARAM_BUILD_EXTRA_ONLY, Flag}) when is_boolean(Flag) ->
    ok;

checkParam({?PT_GP_PARAM_BUILD_EXTRA_ONLY, Flag}) ->
    throw(?mk_parse_error(0, ?PT_GP_ERR_INV_BUILD_EXTRA_ONLY(Flag)));

checkParam({?PT_GP_PARAM_BUILD_EXTRA_PREFIX, Prefix}) when is_atom(Prefix) ->
    ok;

checkParam({?PT_GP_PARAM_BUILD_EXTRA_PREFIX, Prefix}) ->
    throw(?mk_parse_error(0, ?PT_GP_ERR_INV_BUILD_EXTRA_PREFIX(Prefix)));

checkParam({?PT_GP_PARAM_BUILD_EXTRA_POSTFIX, Postfix}) when is_atom(Postfix) ->
    ok;

checkParam({?PT_GP_PARAM_BUILD_EXTRA_POSTFIX, Postfix}) ->
    throw(?mk_parse_error(0, ?PT_GP_ERR_INV_BUILD_EXTRA_POSTFIX(Postfix)));

checkParam({?PT_GP_PARAM_IGNORE, [{_Name, _Ar} | _]}) when is_atom(_Name),
                                                          is_integer(_Ar)->
    ok;

checkParam({?PT_GP_PARAM_IGNORE, Ignore}) ->
    throw(?mk_parse_error(0, ?PT_GP_ERR_INV_IGNORE(Ignore))).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


parseParams(Params) ->
    {?PT_GP_PARAM_MODULE, Module} =
        lists:keyfind(?PT_GP_PARAM_MODULE, 1, Params),

    {?PT_GP_PARAM_MODE, Mode} =
        lists:keyfind(?PT_GP_PARAM_MODE, 1, Params),

    {?PT_GP_PARAM_PROXY, Proxy} =
        lists:keyfind(?PT_GP_PARAM_PROXY, 1, Params),

    {?PT_GP_PARAM_EXTRA_ARGS, Extra} =
        lists:keyfind(?PT_GP_PARAM_EXTRA_ARGS, 1, Params),

    {?PT_GP_PARAM_BUILD_EXTRA_ONLY, Flag} =
        lists:keyfind(?PT_GP_PARAM_BUILD_EXTRA_ONLY, 1, Params),

    {?PT_GP_PARAM_BUILD_EXTRA_PREFIX, Prefix} =
        lists:keyfind(?PT_GP_PARAM_BUILD_EXTRA_PREFIX, 1, Params),

    {?PT_GP_PARAM_BUILD_EXTRA_POSTFIX, Postfix} =
        lists:keyfind(?PT_GP_PARAM_BUILD_EXTRA_POSTFIX, 1, Params),


    {?PT_GP_PARAM_IGNORE, Ignore} =
        lists:keyfind(?PT_GP_PARAM_IGNORE, 1, Params),


    {Module, Mode, Proxy, Extra, Prefix, Postfix, Flag, lists:usort(lists:append(?PT_GP_PARAM_IGNORE_DEFAULT, Ignore))}.
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


checkProxy(AST, ?PT_GP_MODE_FUN, Proxy, Extra, Flag) ->
    ok = internalCheckProxy(AST, Proxy, Extra + 1),

    case Flag of
        true ->
            ok;

        false ->
            internalCheckProxy(AST, Proxy, 1)
    end;

checkProxy(AST, ?PT_GP_MODE_MFA, Proxy, Extra, Flag) ->
    ok = internalCheckProxy(AST, Proxy, Extra + 3),

    case Flag of
        true ->
            ok;

        false ->
            internalCheckProxy(AST, Proxy, 3)
    end.
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

internalCheckProxy(AST, Proxy, Extra) ->
    case pt_lib:match(AST, ast_pattern("$Proxy/$Extra[...$_...]. ")) of

        [_] ->
            ok;

        _ ->
            throw(
              ?mk_parse_error(0, ?PT_GP_ERR_INV_PROXY_ARR(Proxy, Extra)))
    end.
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


readModule(Module, Ignore) ->
    case code:load_file(Module) of
        {module, Module} ->
            Result =
                Module:module_info(exports) -- Ignore,

            code:purge(Module),

            Result;

        {error, What} ->
            throw(
              ?mk_parse_error(0, ?PT_GP_ERR_LOAD_MODULE(Module, What)))
    end.
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

buildProxy(AST, ?PT_GP_MODE_FUN, Cfg, List) ->
    lists:foldl(funBuilder(Cfg), AST, List);

buildProxy(AST, ?PT_GP_MODE_MFA, Cfg, List) ->
    lists:foldl(mfaBuilder(Cfg), AST, List).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


funBuilder({Module, Proxy, Extra, Prefix, Postfix, true}) ->
    fun({F, A}, AST) ->
            buildFun(Module, F, A, Extra, Prefix, Postfix, Proxy, AST)
    end;

funBuilder({Module, Proxy, Extra, Prefix, Postfix, false}) ->
    fun({F, A}, AST) ->
            Tmp = buildFun(Module, F, A, [], '', '', Proxy, AST),
            buildFun(Module, F, A, Extra, Prefix, Postfix, Proxy, Tmp)
    end.
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



mfaBuilder({Module, Proxy, Extra, Prefix, Postfix, true}) ->
    fun({F, A}, AST) ->
            buildMfa(Module, F, A, Extra, Prefix, Postfix, Proxy, AST)
    end;

mfaBuilder({Module, Proxy, Extra, Prefix, Postfix, false}) ->
    fun({F, A}, AST) ->
            Tmp = buildMfa(Module, F, A, [], '', '', Proxy, AST),
            buildMfa(Module, F, A, Extra, Prefix, Postfix, Proxy, Tmp)
    end.
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


buildFun(M, F, A, Extra, Prefix, Postfix, Proxy, AST) when is_list(Extra) ->
    Params =
        [{var, 0, makeParamName(P)}
         || P <- lists:seq(1, A)],

    XParams =
        [{var, 0, XP}
         || XP <- Extra],

    FullParams =
        Params ++ XParams,

    NewFunName = make_new_fun_name(Prefix, F, Postfix),

    Fun =
        ast("@NewFunName(...$FullParams...) -> X = fun() -> @M:@F(...$Params...) end, @Proxy(X, ...$XParams...).", 0),

    pt_lib:add_function(AST, Fun).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


buildMfa(M, F, A, Extra, Prefix, Postfix, Proxy, AST) ->

    Params =
        [{var, 0, makeParamName(P)}
         || P <- lists:seq(1, A)],

    Args =
        lists:foldl(fun(X, Y) ->
                            ast("[$X | $Y].", 0)
                    end,
                    ast("[].", 0),
                    lists:reverse(Params)),

    XParams =
        [{var, 0, XP}
         || XP <- Extra],

    FullParams =
        Params ++ XParams,

    NewFunName = make_new_fun_name(Prefix, F, Postfix),

    Fun =
        ast("@NewFunName(...$FullParams...) -> @Proxy(@M, @F, $Args, ...$XParams...).", 0),

    pt_lib:add_function(AST, Fun).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


makeParamName(Param) ->
    List = lists:flatten(io_lib:format("P~w", [Param])),
    try
        erlang:list_to_existing_atom(List)
    catch
        _:_ ->
            erlang:list_to_atom(List)
    end.
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

make_new_fun_name(Prefix, F, Postfix) when is_atom(Prefix),
                                           is_atom(F),
                                           is_atom(Postfix) ->
    case {Prefix, Postfix} of
        {'', ''} ->
            F;
        _ ->
            PrefixStr = erlang:atom_to_list(Prefix),
            PostfixStr = erlang:atom_to_list(Postfix),
            FStr = erlang:atom_to_list(F),
            NewF = string:join([PrefixStr, FStr, PostfixStr], ""),
            erlang:list_to_atom(NewF)
    end;

make_new_fun_name(Prefix, F, Postfix) ->
    throw({badarg, {Prefix, F, Postfix}}).