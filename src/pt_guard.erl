%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Nikita Roshchupkin, Timofey Barmin,
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%%     Allow you to use any function in guard tests.
%%%     Example:
%%%         NameFunction(...$Params...) ->
%%%             case ...$Params... of
%%%                 return_true ->
%%%                     true;
%%%                 rest_of_it ->
%%%                     false
%%%             end
%%%         handle_call(...) when pt_guard(NameFunction(...$Params...))
%%%
%%%     simply creates new function, calls new function and call user's function when succ
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pt_guard).

-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_lib/include/pt_error_macro.hrl").

-export([parse_transform/2, format_error/1]).

parse_transform(AST, _Options) ->
    try
        {NewAST, NewFunctions} =
        pt_lib:replace_fold(AST, {ast_pattern("$F[...$Clauses...]."), Acc},
            begin
                {NewClauses, NewF} = process_clauses(F, lists:reverse(Clauses), {0, [], []}),
                [{_, Line, _, _, _} | _] = NewClauses,
                {ast("$F[...$NewClauses...].",Line), NewF ++ Acc}
            end, []),

        lists:foldl(
            fun ({FF, FFClauses}, NewASTAcc) ->
                [{_, Line, _, _, _} | _] = FFClauses,
                pt_lib:add_local_function(NewASTAcc, ast("$FF[...$FFClauses...].", Line))
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
    {NewN, NeedProcess} = process_guards(Guards, N),
    case NeedProcess of
        false -> process_clauses(F, T, {NewN, [Clause|ProcessedClauses], NewFunctions});
        true  ->
            FN = erlang:list_to_atom(erlang:atom_to_list(F) ++ "_" ++ erlang:integer_to_list(Line) ++ "_PT_AUTO_GENERATED_FUN"),
            {P, _} = lists:mapfoldl(
                fun (Param, K) ->
                    Var = {var, 0, erlang:list_to_atom("PT_AUTO_VAR_NAME_" ++ erlang:integer_to_list(K))},
                    {{ast("$Var = $Param.", 0), Var}, K + 1}
                end, 0, Params),
            {NewParamsPatterns, NewParamsValues} = lists:unzip(P),
            Guards2 = change_guard(Guards),
            GuardCheck = zip_guards(Guards2),
            NewBody = [ast("case $GuardCheck of true  -> ...$Body... ; _ -> @FN(...$NewParamsValues...) end.", Line)],

            NewClause = ast("(...$NewParamsPatterns...) -> ...$NewBody... .", Line),

            NewFunctionClauses =
                case ProcessedClauses of
                    [] -> [ast("(...$Params...) when true == false -> never_happen.", 0)];
                    _  -> ProcessedClauses
                end,

            process_clauses(F, T, {NewN, [NewClause], [{FN, NewFunctionClauses}|NewFunctions]})
    end.

change_guard([[{call, _, {atom, _, pt_guard}, Return_guards}]]) ->
    [Return_guards];
change_guard(Guards) ->
    Guards.

process_guards(Guards, N) ->
    NeedProcess = lists:foldr(
            fun (Guard, NP) ->
                {_, NP2} =
                    pt_lib:replace_fold(Guard,
                        {ast_pattern("pt_guard(...$_...).", Line), _},
                        {ast("[].", Line), true}, false),
                case NP2 of
                    true  -> true;
                    false -> NP
                end
            end, false, Guards),

    case NeedProcess of
        true  -> {N + 1, NeedProcess};
        false -> {N,     NeedProcess}
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