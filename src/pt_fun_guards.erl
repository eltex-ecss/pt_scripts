%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Nikita Roshchupkin, Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%     Allow you to use lists:member in guard tests.
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
                {NewClauses, NewF} = process_clauses_func(F, lists:reverse(Clauses), {0, [], []}),
                MoreNewClauses = process_clauses_case(lists:reverse(NewClauses), []),
                [{_, Line, _, _, _} | _] = MoreNewClauses,
                {ast("$F[...$MoreNewClauses...].",Line), NewF ++ Acc}
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

process_clauses_case([], Clauses) -> Clauses;
process_clauses_case([ast_pattern("(...$Params...) when ...$Guards... -> ...$Body... .",  Line) = _|T], ProcessedClauses) ->
    NewBody = process_body(Body),
    NewClause = ast("(...$Params...) when ...$Guards... -> ...$NewBody... .", Line),
    process_clauses_case(T, [NewClause|ProcessedClauses]).

process_body(Body) ->
    {NewBody, _} = lists:foldr(
        fun (BodyCase, {Acc, NP}) ->
            {NewGuard, NP2} =
                pt_lib:replace_fold(BodyCase,
                    {ast_pattern("case $Params of ...$OtherData...  end.", Line), _},
                    begin
                        NewOtherData = process_case(OtherData, []),
                        {ast("case $Params of ...$NewOtherData... end.", Line), true}
                    end, false),
                case NP2 of
                    true  -> {[NewGuard|Acc], unrolling_list_member};
                    false -> {[NewGuard|Acc], NP}
                end
        end, {[], false}, Body),
    NewBody.

process_case([{_A1, _A2, _A3, Guards, _A4} | TailListClause], Acc) ->
    {_, NewGuards, NeedProcess} = process_guards(Guards, 1),
    case NeedProcess of
        unrolling_list_member ->
            process_case(TailListClause, [{_A1, _A2, _A3, NewGuards, _A4} | Acc]);
        _ ->
            process_case(TailListClause, [{_A1, _A2, _A3, Guards, _A4} | Acc])
    end;
process_case(_, Acc) ->
    lists:reverse(Acc).

process_clauses_func(_F, [], {_, Clauses, NewF}) -> {Clauses, NewF};
process_clauses_func(F, [ast_pattern("(...$Params...) when ...$Guards... -> ...$Body... .",  Line) = Clause|T], {N, ProcessedClauses, NewFunctions}) ->
    {NewN, NewGuards, NeedProcess} = process_guards(Guards, N),
    case NeedProcess of
        false -> process_clauses_func(F, T, {NewN, [Clause|ProcessedClauses], NewFunctions});
        true  ->
            FN = erlang:list_to_atom(erlang:atom_to_list(F) ++ "_" ++ erlang:integer_to_list(Line) ++ "_PT_AUTO_GENERATED_FUN"),
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
            process_clauses_func(F, T, {NewN, [NewClause, NextClause], [{FN, NewFunctionClauses}|NewFunctions]});
        unrolling_list_member ->
             NewClause =
                ast("(...$Params...) when ...$NewGuards... -> ...$Body... .", Line),
             process_clauses_func(F, T, {NewN, [NewClause|ProcessedClauses], NewFunctions})
    end.

process_guards(Guards, N) ->
    ListPatterns = cut_title(pt_lib:match(Guards,
        ast_pattern("lists:member($_Var, $_ListAst).")), []),
    ListBool = lists:map(
        fun(Acc) -> check_const_list(Acc, true) end, ListPatterns),
    {NewGuards, NeedProcess} =
        case proplists:is_defined(false, ListBool) of
            false ->
                unrolling_list(Guards);
            true ->
                replace_list(Guards)
        end,
    case NeedProcess of
        unrolling_list_member -> {N, NewGuards, NeedProcess};
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

cut_title([{_, _, {remote,_,{_,_,lists},
        {_,_,member}}, [_ | TailVars]} | TailListPatterns], Acc) ->
    cut_title(TailListPatterns, [TailVars | Acc]);
cut_title(_, Acc) ->
    Acc.

check_const({Type, _, _, _}, _) when Type =:= var orelse Type =:= call ->
    false;
check_const({Type, _, _}, _) when Type =:= var orelse Type =:= call ->
    false;
check_const({_, _, Vars, Tail}, true) when is_list(Vars) ->
    Flag = check_const_list(Vars, true),
    check_const(Tail, Flag);
check_const({_, _, Vars, Tail}, true) ->
    Flag = check_const(Vars, true),
    check_const(Tail, Flag);
check_const({_, _, Vars}, true) when is_list(Vars) ->
    check_const_list(Vars, true);
check_const({_, _, Vars}, true) ->
    check_const(Vars, true);
check_const(_, false) ->
    false;
check_const(_, _) ->
    true.

check_const_list([Var | Tail], true) ->
    Flag = check_const(Var, true),
    check_const_list(Tail, Flag);
check_const_list(_, false) ->
    false;
check_const_list(_, _) ->
    true.

const_expression({_a, _b, Var}, [Head | Tail], Acc) when Tail =/= [] ->
    IOList = io_lib:format("~s =:= ~p orelse ", [atom_to_list(Var), Head]),
    FlatList = lists:flatten(IOList),
    const_expression({_a, _b, Var}, Tail, Acc ++ FlatList);
const_expression({_a, _b, Var}, [Head | Tail], Acc) ->
    IOList = io_lib:format("~s =:= ~p.", [atom_to_list(Var), Head]),
    FlatList = lists:flatten(IOList),
    const_expression({_a, _b, Var}, Tail, Acc ++ FlatList);
const_expression(_, [], Acc) ->
    Acc.

unrolling_list(Guards) ->
    lists:foldr(
        fun (Guard, {Acc, NP}) ->
            {NewGuard, NP2} =
                pt_lib:replace_fold(Guard,
                    {ast_pattern("lists:member($Var, $ListAst).", Line), _},
                    begin
                        {value, ValueList, _Bs} =
                            erl_eval:exprs([ListAst], erl_eval:new_bindings()),
                        String = const_expression(Var, ValueList, []),
                        [Ast] = pt_lib:str2ast(String, Line),
                        {Ast, true}
                    end, false),
                case NP2 of
                    true  -> {[NewGuard|Acc], unrolling_list_member};
                    false -> {[NewGuard|Acc], NP}
                end
        end, {[], false}, Guards).

replace_list(Guards) ->
    lists:foldr(
        fun (Guard, {Acc, NP}) ->
            {NewGuard, NP2} =
                pt_lib:replace_fold(Guard,
                    {ast_pattern("lists:member($_, $L).", Line), _},
                    {ast("is_list($L).", Line), true}, false),
                case NP2 of
                    true  -> {[NewGuard|Acc], true};
                    false -> {[NewGuard|Acc], NP}
                end
        end, {[], false}, Guards).