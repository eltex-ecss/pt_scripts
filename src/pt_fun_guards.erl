%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%%     Allow you to use lists:member in guard tests.
%%%
%%%     simply creates new function, calls lists:member and call user's function when succ
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pt_fun_guards).

-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_lib/include/pt_error_macro.hrl").

-export([parse_transform/2, format_error/1]).

parse_transform(AST, _Options) ->
    try
        {NewAST, NewFunctions} =
        pt_lib:replace_fold(AST, {ast_pattern("$F[...$Clauses...]."), Acc},
            begin
                {NewClauses, NewF} = process_clauses(F, lists:reverse(Clauses), {0, [], []}),
                {ast("$F[...$NewClauses...].",0), NewF ++ Acc}
            end, []),

        lists:foldl(
            fun ({FF, FFClauses}, NewASTAcc) ->
                pt_lib:add_function(NewASTAcc, ast("$FF[...$FFClauses...].", 0))
            end, NewAST, NewFunctions)

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


process_clauses(_F, [], {_, Clauses, NewF}) -> {Clauses, NewF};
process_clauses(F, [ast_pattern("(...$Params...) when ...$Guards... -> ...$Body... .",  Line) = Clause|T], {N, ProcessedClauses, NewFunctions}) ->
    {NewN, NewGuards, NeedProcess} = process_guards(Guards, N),
    case NeedProcess of
        false -> process_clauses(F, T, {NewN, [Clause|ProcessedClauses], NewFunctions});
        true  ->
            FN = erlang:list_to_atom(erlang:atom_to_list(F) ++ "_" ++ erlang:integer_to_list(NewN) ++ "_PT_AUTO_GENERATED_FUN"),
            {P, _} = lists:mapfoldl(
                fun (Param, K) ->
                    Var = {var, 0, erlang:list_to_atom("PT_AUTO_VAR_NAME_" ++ erlang:integer_to_list(K))},
                    {{ast("$Var = $Param.", 0), Var}, K + 1}
                end, 0, Params),
            {NewParamsPatterns, NewParamsValues} = lists:unzip(P),

            GuardCheck = zip_guards(Guards),

            NewBody = [ast("case $GuardCheck of true  -> ...$Body... ; false -> @FN(...$NewParamsValues...) end.", Line)],

            NewClause = ast("(...$NewParamsPatterns...) when ...$NewGuards... -> ...$NewBody... .", Line),

            NextClause = ast("(...$NewParamsValues...) -> @FN(...$NewParamsValues...).", 0),

            NewFunctionClauses =
                case ProcessedClauses of
                    [] -> [ast("(...$Params...) when true == false -> never_happen.", 0)];
                    _  -> ProcessedClauses
                end,

            process_clauses(F, T, {NewN, [NewClause, NextClause], [{FN, NewFunctionClauses}|NewFunctions]})
    end.


process_guards(Guards, N) ->
    {NewGuards, NeedProcess} = lists:foldr(
            fun (Guard, {Acc, NP}) ->
                {NewGuard, NP2} =
                    pt_lib:replace_fold(Guard,
                        {ast_pattern("lists:member($_, $L).", Line), _},
                        {ast("is_list($L).", Line), true}, false),
                case NP2 of
                    true  -> {[NewGuard|Acc], true};
                    false -> {[NewGuard|Acc], NP}
                end
            end, {[], false}, Guards),

    case NeedProcess of
        true  -> {N + 1, NewGuards, NeedProcess};
        false -> {N,     NewGuards, NeedProcess}
    end.

zip_guards([Guard]) ->
    zip_guard(Guard);
zip_guards([Guard1|T]) ->
    ZG1 = zip_guard(Guard1),
    lists:foldl(
        fun (Guard, Acc) ->
            ZG = zip_guard(Guard),
            ast("$ZG orelse $Acc.", 0)
        end, ZG1, T).

zip_guard([Guard]) -> Guard;
zip_guard([Guard|T]) ->
    lists:foldl(
        fun (G, Acc) ->
            ast("$G andalso $Acc.", 0)
        end, Guard, T).

format_error(Unknown) ->
    io_lib:format("Unknown error: ~p", [Unknown]).