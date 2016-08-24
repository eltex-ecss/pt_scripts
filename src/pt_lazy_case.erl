%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Nikita Roshchupkin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%% @end
%%% Created : Aug 2016
%%%-------------------------------------------------------------------
-module(pt_lazy_case).

-export([parse_transform/2, format_error/1]).

-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_lib/include/pt_error_macro.hrl").
-include_lib("pt_lib/include/pt_patrol.hrl").

-patrol([{tty, error}]).

parse_transform(AST, _) ->
    ASTFunction = parser_function(AST, []),
    File = pt_lib:get_file_name(AST),
    F = fun(Head, Acc) ->
        [parser_case(Head, File) | Acc]
    end,
    ASTCase = lists:foldl(F, [], ASTFunction),
    NewASTFunction = replace_function(AST, ASTCase, []),
    NewASTFunction.

parser_function([{function, _, _, _, DataAST} | TailAST], Acc) ->
    parser_function(TailAST, [DataAST | Acc]);
parser_function([_ | TailAST], Acc) ->
    parser_function(TailAST, Acc);
parser_function(_, Acc) ->
    Acc.

replace_line_ast({bin, _, V1}) ->
    {bin, 0, replace_line_ast(V1)};
replace_line_ast({bin_element, _, V1, V2, V3}) ->
    {cons, 0, replace_line_ast(V1), replace_line_ast(V2), replace_line_ast(V3)};
replace_line_ast({cons, _, V1, V2}) ->
    {cons, 0, replace_line_ast(V1), replace_line_ast(V2)};
replace_line_ast({call, _, V1, V2}) ->
    {call, 0, replace_line_ast(V1), replace_line_ast(V2)};
replace_line_ast({remote, _, V1, V2}) ->
    {remote, 0, replace_line_ast(V1), replace_line_ast(V2)};
replace_line_ast({map, _, V1, V2}) ->
    {map, 0, replace_line_ast(V1), replace_line_ast(V2)};
replace_line_ast({nil, _}) ->
    {nil, 0};
replace_line_ast({op, _, Op, V1, V2}) ->
    {op, 0, Op, replace_line_ast(V1), replace_line_ast(V2)};
replace_line_ast({op, _, Op, V1}) ->
    {op, 0, Op, replace_line_ast(V1)};
replace_line_ast({record, _, Name, V1}) ->
    {record, 0, Name, replace_line_ast(V1)};
replace_line_ast({record_field, _, V1, V2}) ->
    {record_field, 0, replace_line_ast(V1), replace_line_ast(V2)};
replace_line_ast({record_field, _, V1, Name, V2}) ->
    {record_field, 0, replace_line_ast(V1), Name, replace_line_ast(V2)};
replace_line_ast({record_index, _, Name, V1}) ->
    {record_index, 0, Name, replace_line_ast(V1)};
replace_line_ast({tuple, _, V1}) ->
    {tuple, 0, replace_line_ast(V1)};
replace_line_ast({var, _, A}) ->
    {var, 0, A};
replace_line_ast({atom, _, L}) ->
    {atom, 0, L};
replace_line_ast({char, _, L}) ->
    {char, 0, L};
replace_line_ast({float, _, L}) ->
    {float, 0, L};
replace_line_ast({integer, _, L}) ->
    {integer, 0, L};
replace_line_ast({string, _, L}) ->
    {string, 0, replace_line_ast(L)};
replace_line_ast([V1 | Tail]) ->
    [replace_line_ast(V1) | replace_line_ast(Tail)];
replace_line_ast([]) ->
    [];
replace_line_ast(V1) ->
    V1.

parser_case([{atom, Line, '@lazy_case'} | TailAST], File) ->
    case TailAST of
        [{match, L, V, {'case', _, _, _} = CaseAST} | TailAST2] ->
            [{match, L, V, replace_case(parser_case(CaseAST, File), File)} | TailAST2];
        {match, L, V, {'case', _, _, _} = CaseAST} ->
            {match, L, V, replace_case(parser_case(CaseAST, File), File)};
        [{'case', _, _, _} | _] ->
            replace_case(parser_case(TailAST, File), File);
        {'case', _, _, _} ->
            replace_case(parser_case(TailAST, File), File);
        _ ->
            {ok, Cwd} = file:get_cwd(),
            FullFile = filename:join(Cwd, File),
            io:format("~ts:~p: Warning: lazy_case did not work ~n", [FullFile, Line]),
            parser_case(TailAST, File)
    end;
parser_case({V1, V2, V3, V4, TailAST}, File) ->
    {V1, V2, V3, V4, parser_case(TailAST, File)};
parser_case({V1, V2, V3, TailAST}, File) ->
    {V1, V2, V3, parser_case(TailAST, File)};
parser_case({V1, V2, TailAST}, File) ->
    {V1, V2, parser_case(TailAST, File)};
parser_case({V1, TailAST}, File) ->
    {V1, parser_case(TailAST, File)};
parser_case([{V1, V2, V3, V4, TailAST} | Other1AST], File) ->
    [{V1, V2, V3, V4, parser_case(TailAST, File)} | parser_case(Other1AST, File)];
parser_case([{V1, V2, V3, TailAST} | Other1AST], File) ->
    [{V1, V2, V3, parser_case(TailAST, File)} | parser_case(Other1AST, File)];
parser_case([{V1, V2, TailAST} | Other1AST], File) ->
    [{V1, V2, parser_case(TailAST, File)} | parser_case(Other1AST, File)];
parser_case([{V1, TailAST} | Other1AST], File) ->
    [{V1, parser_case(TailAST, File)} | parser_case(Other1AST, File)];
parser_case([V1 | TailAST], File) ->
    [V1 | parser_case(TailAST, File)];
parser_case(Data, _) ->
    Data.

parse_clause([{clause, V1, [{var, V2, D1}], Guard, _} = Data | TailASTClause], OtherAST, AccGuard) ->
    case {TailASTClause, Guard} of
        {[], []} ->
            parse_clause(TailASTClause, [Data | OtherAST], AccGuard);
        {_, []} ->
            throw(?mk_parse_error(V1, {clause_always, {var, V2, D1}}));
        {_, _} ->
            case check_guard(Guard, AccGuard, []) of
                {true, NewAccGuard} ->
                    parse_clause(TailASTClause, [Data | OtherAST], NewAccGuard);
                _ ->
                    throw(?mk_parse_error(V1, {clause_always, {var, V2, D1}}))
            end
    end;
parse_clause([{clause, V1, [{tuple, _, [Elem | []]}], V3, V4} | TailASTClause], OtherAST, AccGuard) ->
    parse_clause(TailASTClause, [{clause, V1, [Elem], V3, V4} | OtherAST], AccGuard);
parse_clause([{clause, V, Data, _, _} | _], _, _) ->
    throw(?mk_parse_error(V, {clause_invalid, Data}));
parse_clause(_, OtherAST, _) ->
    lists:reverse(OtherAST).

parse_clause([{clause, V1, [{var, V2, D1}], Guard, _} = Data | TailASTClause], Acc, OtherAST, N, AccGuard) ->
    case {TailASTClause, Guard} of
        {[], []} ->
            parse_clause(TailASTClause, [{N, {guard, Guard}, {var, V2, D1}} | Acc], [{N, Data} | OtherAST], N + 1, AccGuard);
        {_, []} ->
            throw(?mk_parse_error(V1, {clause_always, {var, V2, D1}}));
        {_, _} ->
            case check_guard(replace_line_ast(Guard), AccGuard, []) of
                {true, NewAccGuard} ->
                    parse_clause(TailASTClause, [{N, {guard, Guard}, {var, V2, D1}} | Acc], [{N, Data} | OtherAST], N + 1, NewAccGuard);
                _ ->
                    throw(?mk_parse_error(V1, {clause_always, {var, V2, D1}}))
            end
    end;
parse_clause([{clause, V1, [{tuple, V2, [Elem | TailElem]}], [], V4} | TailASTClause], Acc, OtherAST, N, AccGuard) ->
    parse_clause(TailASTClause, [{N, {tuple, []}, Elem} | Acc], [{N, {clause, V1, [{tuple, V2, TailElem}], [], V4}} | OtherAST], N + 1, AccGuard);
parse_clause([{clause, _, [{tuple, _, _}], _, _} | _], _, _, _, _) ->
    {false, invalid_guard};
parse_clause([{clause, V, Data, _, _} | _], _, _, _, _) ->
    throw(?mk_parse_error(V, {clause_invalid, Data}));
parse_clause(_, Acc, OtherAST, _, _) ->
    {lists:reverse(OtherAST), lists:reverse(Acc)}.

parse_tuple(HeadAstTuple, ListASTClause, [], V1, _) ->
    NewListASTClause = parse_clause(ListASTClause, [], []),
    OffRepeateASTClause = delete_repeate(NewListASTClause, []),
    case check_load_code(OffRepeateASTClause) of
        false ->
            {'case', V1, HeadAstTuple, OffRepeateASTClause};
        {true, Data} ->
            Data
    end;
parse_tuple(HeadAstTuple, ListASTClause, TailAstTuple, V1, File) ->
    case parse_clause(ListASTClause, [], [], 1, []) of
        {false, invalid_guard} ->
            {'case', V1, {tuple, V1, [HeadAstTuple | TailAstTuple]}, ListASTClause};
        {NewListASTClause, ListElems} ->
            CountElems = count_clause(ListElems, []),
            OffRepeateASTClause = delete_repeate(create_new_case(NewListASTClause, CountElems, TailAstTuple, [], V1), []),
            NewCase = {'case', V1, HeadAstTuple, OffRepeateASTClause},
            ChangeCase = parser_case(NewCase, File),
            case check_load_code(ChangeCase) of
                false ->
                    ChangeCase;
                {true, Data} ->
                    Data
            end
    end.

replace_case([{'case', V1, {tuple, _, [HeadAstTuple | TailAstTuple]}, ListASTClause}], File) ->
    NewCase = parse_tuple(HeadAstTuple, ListASTClause, TailAstTuple, V1, File),
    case is_list(NewCase) of
        true ->
            NewCase;
        _ ->
            [NewCase]
    end;
replace_case({'case', V1, {tuple, _, [HeadAstTuple | TailAstTuple]}, ListASTClause}, File) ->
    parse_tuple(HeadAstTuple, ListASTClause, TailAstTuple, V1, File);
replace_case(Data, _) ->
    Data.

replace_function([{function, V1, V2, V3, _} | TailAST], [HeadASTCase | TailASTCase], Acc) ->
    replace_function(TailAST, TailASTCase, [{function, V1, V2, V3, HeadASTCase} | Acc]);
replace_function([Data | TailAST], ASTCase, Acc) ->
    replace_function(TailAST, ASTCase, [Data | Acc]);
replace_function(_, _, Acc) ->
    lists:reverse(Acc).

count_clause([{_, {Type, Guard1}, Elem} = AccElem | TailElem], Acc) ->
    NewData1 = replace_line_ast(Elem),
    NewGuard1 = replace_line_ast(Guard1),
    F1 = fun
        ({Ind, {_, Guard2}, {var, _, _}}) ->
            NewGuard2 = replace_line_ast(Guard2),
            case NewGuard1 =:= NewGuard2 of
                true ->
                    {true, Ind};
                false ->
                    case Type of
                        guard -> false;
                        tuple -> {true, Ind}
                    end
            end;
        ({Ind, {_, Guard2}, Data2}) ->
            NewData2 = replace_line_ast(Data2),
            NewGuard2 = replace_line_ast(Guard2),
            case {NewGuard1 =:= NewGuard2, NewData2 =:= NewData1} of
                {true, true} ->
                    {true, Ind};
                _ ->
                    false
            end
    end,
    F2 = fun
        ({_, {_, Guard2}, Data2} = ASTData) ->
            NewData2 = replace_line_ast(Data2),
            NewGuard2 = replace_line_ast(Guard2),
            case {NewGuard1 =:= NewGuard2, NewData2 =:= NewData1} of
                {true, true} ->
                    false;
                _ ->
                    {true, ASTData}
            end
    end,
    ListCountElem = lists:filtermap(F1, [AccElem | TailElem]),
    NewTailElem = lists:filtermap(F2, TailElem),
    count_clause(NewTailElem, [{ListCountElem, Guard1, Elem} | Acc]);
count_clause(_, Acc) ->
    lists:reverse(Acc).

create_new_case(ListASTClause, [{ListCountElem, Guard1, Elem} | TailElems], TailAstTuple, Acc1, V1) ->
    F = fun(CountElem, Acc) ->
        {_, ASTClause} = lists:keyfind(CountElem, 1, ListASTClause),
        [ASTClause | Acc]
    end,
    NewListASTClause = lists:reverse(lists:foldl(F, [], ListCountElem)),
    NewClause = {clause, V1, [Elem], Guard1, [{atom, V1, '@lazy_case'}, {'case', V1, {tuple, V1, TailAstTuple}, NewListASTClause}]},
    create_new_case(ListASTClause, TailElems, TailAstTuple, [NewClause | Acc1], V1);
create_new_case(_, _, _, Acc1, _) ->
    lists:reverse(Acc1).

check_guard(Guard, [Guard | _], _) ->
    false;
check_guard(Guard, [HeadGuard | TailGuard], Acc) ->
    check_guard(Guard, TailGuard, [HeadGuard | Acc]);
check_guard(Guard, _, Acc) ->

    {true, [Guard | Acc]}.

check_load_code({'case', _, _, [{clause, _, [{var, _, '_'}], _, D3} | _]}) ->
    {true, D3};
check_load_code([{clause, _, [{var, _, '_'}], _, D3} | _]) ->
    {true, D3};
check_load_code(_) ->
    false.

delete_repeate([{clause, _, Data1, Guard1, _} = HeadASTClause | TailASTClause], Acc) ->
    NewGuard1 = replace_line_ast(Guard1),
    NewData1 = replace_line_ast(Data1),
    F = fun ({clause, _, Data2, Guard2, _} = ASTClause) ->
        NewGuard2 = replace_line_ast(Guard2),
        NewData2 = replace_line_ast(Data2),
        case {NewGuard2 =:= NewGuard1, NewData2 =:= NewData1} of
            {true, true} ->
                false;
            _ ->
                {true, ASTClause}
        end
    end,
    NewTailASTClause = lists:filtermap(F, TailASTClause),
    delete_repeate(NewTailASTClause, [HeadASTClause | Acc]);
delete_repeate(_, Acc) ->
    lists:reverse(Acc).

format_error({clause_always, Error}) ->
    io_lib:format("~p: clause is always satisfied ~p", [error, Error]);
format_error({clause_invalid, Error}) ->
    io_lib:format("~p: clause contains invalid data ~p", [error, Error]);
format_error({case_invalid, Error}) ->
    io_lib:format("~p: case contains invalid data ~p", [error, Error]);
format_error(Unknown) ->
    io_lib:format("Unknown error: ~p", [Unknown]).