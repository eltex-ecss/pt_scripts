%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% Add some usefull macros
%%% Usage:
%%%  -include("pt_scripts/include/pt_macro.hrl").
%%%
%%%  ...
%%%
%%%  ?FUNC        == 'my_fun/4', % atom 'cur_fun/arity'
%%%  ?FUNC_NAME   == 'my_fun,    % atom 'cur_fun'
%%%  ?FUNC_STRING == "my_fun/4", % string "cur_fun/arity"
%%%  ?ARGS_LIST   == [Arg1, Arg2,...] % Arguments of current function
%%%  ?REGEXP(Expr) % Compiled at compile time regex
%%%  ?REGEXP_OPT(Expr, Options) % Like previous but use Options for re:compile
%%%  ...
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pt_macro).

-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_lib/include/pt_error_macro.hrl").

-export([parse_transform/2, define/1, format_error/1]).


parse_transform(AST, _Options) ->
    try
        AST1 = lists:map(
                    fun
                        ({function, Line2, FunName, FunArity, _} = Tree) ->
                            Tree1 = pt_lib:replace(
                                Tree,
                                ast_pattern("pt_macro_define(function).", Line),
                                {atom, Line, pt_supp:mk_atom(atom_to_list(FunName) ++ "/", FunArity)}),

                            Tree2 = pt_lib:replace(
                                Tree1,
                                ast_pattern("pt_macro_define(function_name).", Line),
                                {atom, Line, FunName}),

                            Tree3 = pt_lib:replace(
                                Tree2,
                                ast_pattern("pt_macro_define(function_string).", Line),
                                {string, Line, atom_to_list(FunName) ++ "/" ++ integer_to_list(FunArity)}),

                            {function, _, _, _, Clauses} = Tree3,

                            NewClauses =
                                lists:map(
                                    fun (ast_pattern("(...$Args...) when ...$Guards... -> ...$Expr... .", LL) = C1) ->
                                        case pt_lib:replace(Expr, ast_pattern("pt_macro_define(args_list)."), list_to_astlist(Args)) of
                                            Expr -> C1;
                                            NewExpr ->
                                                {NewArgs, _} = lists:mapfoldl(fun (AA, N) -> VN = mk_var_name(N), {ast("$VN = $AA.", LL), N+1} end, 0, Args),
                                                ast("(...$NewArgs...) when ...$Guards... -> ...$NewExpr... .", LL)
                                        end
                                    end, Clauses),

                            Tree4 = {function, Line2, FunName, FunArity, NewClauses},

                            Tree5 = pt_lib:replace(
                                Tree4,
                                ast("pt_macro_define(regexp, $Str, $Options).", Line),
                                begin
                                    case {pt_lib:is_term(Str), pt_lib:is_term(Options)} of
                                        {true, true} ->
                                            case catch re:compile(erl_parse:normalise(Str), erl_parse:normalise(Options)) of
                                                {ok, MP} ->
                                                    ast("@MP.", Line);
                                                {error, Error} ->
                                                    throw(?mk_parse_error(Line, {compile_re_error, Error}));
                                                {'EXIT', {badarg, [{re, compile, _} | _]}} ->
                                                    throw(?mk_parse_error(Line, {compile_re_error, bad_param}));
                                                Err ->
                                                    throw(?mk_parse_error(Line, {compile_re_error, Err}))
                                            end;
                                        {false, _} ->
                                            throw(?mk_parse_error(Line, bad_regexp));
                                        {true, false} ->
                                            throw(?mk_parse_error(Line, bad_regexp_options))
                                    end
                                end),

                            Tree5;
                        (T) -> T
                    end,
                    AST),
        AST1
    catch
        throw:{parse_error, _} = Error ->
            pt_supp:print_error(AST, Error),
            exit(Error);
        throw:{error, _} = Error ->
            pt_supp:print_error(AST, Error),
            exit(Error);
        throw:{internal_error, _} = Error ->
            pt_supp:print_error(AST, Error),
            exit(Error)
    end.

define(_) ->
    erlang:error(undef).

mk_var_name(N) ->
    {var, 0, erlang:list_to_atom(lists:flatten(io_lib:format("PT_MACTO_AUTO_VAR_~b", [N])))}.

list_to_astlist(L) -> list_to_astlist_(L, {nil, 0}, 0).

list_to_astlist_([], AST, _N) -> pt_lib:list_reverse(AST);
list_to_astlist_([_|T], AST, N) -> list_to_astlist_(T, {cons, 0, mk_var_name(N), AST}, N + 1).

format_error({compile_re_error, Cause}) ->
    io_lib:format("Error while compiling regexp: ~p", [Cause]);
format_error(bad_regexp) ->
    "regexp should be a term :P";
format_error(bad_regexp_options) ->
    "regexp options should be a term :P";
format_error(Unknown) ->
    io_lib:format("Unknown error: ~p", [Unknown]).