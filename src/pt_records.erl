%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin <timofey.barmin@gmail.com>
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%%     Allow you to access record fields in case if don't know record type at compile time
%%%     See pt_records_tests.
%%%     This code is not used anywhere yet, so i guess there are a lot of errors in this module.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(pt_records).

-export([parse_transform/2, format_error/1]).

-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_lib/include/pt_patrol.hrl").
-include("pt_macro.hrl").
-include_lib("pt_lib/include/pt_error_macro.hrl").

parse_transform(AST, _Options) ->
    RInfo = [ {Name, [ element(3, element(3, F)) || F <- Fields ]} || {Name, Fields} <- pt_lib:get_attribute_value(record, AST) ],
    case RInfo of
        [] -> ?PATROL_INFO("No records found", []), AST;
        _ ->
            ?PATROL_INFO("Records:~n~p~n", [RInfo]),
            AST1 = process_record_field(AST, RInfo),
            AST2 = process_record_index(AST1, RInfo),
            AST3 = process_rvalue_records(AST2, RInfo),
            AST4 = process_lvalue_records(AST3, RInfo),
            AST4
    end.

process_record_field(AST, RInfo) ->
    pt_lib:replace(AST,
        ast_pattern("$Var#$Record.$Field.", Line) = R, is_atom(Record) and pt_lib:is_atom(Field),
        begin
            case {is_var(Record, Line), is_var(Field, Line)} of
                {false, false} -> R;
                {false, FieldVar} ->
                    case lists:keyfind(Record, 1, RInfo) of
                        {_, FList} ->
                            Clauses =
                                [ ast("(@F) -> $Var#$Record.@F.", Line) ||  F <- FList ],
                            ast("case $FieldVar of ...$Clauses... end.", Line);
                        _ -> R
                    end;
                {RecordVar, false} ->
                    ?PATROL_INFO("got record2 ~p#~p.~p", [Var, Record, Field]),
                    Clauses =
                        [ ast("(@Name) -> $Var#@Name.$Field.", Line) ||  {Name, L} <- RInfo, lists:member(erl_parse:normalise(Field), L) ],
                    case Clauses of
                        [] -> throw(?mk_parse_error(Line, {unknown_field, Field}));
                        _ ->
                            ast("case $RecordVar of ...$Clauses... end.", Line)
                    end;
                {RecordVar, FieldVar} ->
                    ?PATROL_INFO("got record3 ~p#~p.~p", [Var, RecordVar, FieldVar]),
                    generate_megacase(Var, RecordVar, FieldVar, Line, RInfo)
            end
        end
    ).

process_record_index(AST, RInfo) ->
    pt_lib:replace(AST,
        ast_pattern("#$Record.$Field.", Line) = R, is_atom(Record) and pt_lib:is_atom(Field),
        begin
            case {is_var(Record, Line), is_var(Field, Line)} of
                {false, false} -> R;
                {false, FieldVar} ->
                    case lists:keyfind(Record, 1, RInfo) of
                        {_, FList} ->
                            Clauses =
                                [ ast("(@F) -> #$Record.@F.", Line) ||  F <- FList ],
                            ast("case $FieldVar of ...$Clauses... end.", Line);
                        _ -> R
                    end;
                {RecordVar, false} ->
                    ?PATROL_INFO("got record2 #~p.~p", [Record, Field]),
                    Clauses =
                        [ ast("(@Name) -> #@Name.$Field.", Line) ||  {Name, L} <- RInfo, lists:member(erl_parse:normalise(Field), L) ],
                    case Clauses of
                        [] -> throw(?mk_parse_error(Line, {unknown_field, Field}));
                        _ ->
                            ast("case $RecordVar of ...$Clauses... end.", Line)
                    end;
                {RecordVar, FieldVar} ->
                    ?PATROL_INFO("got record3 #~p.~p", [RecordVar, FieldVar]),
                    generate_megacase(false, RecordVar, FieldVar, Line, RInfo)
            end
        end
    ).

process_rvalue_records(AST, RInfo) ->
    pt_lib:replace(AST,
        [
        {
            ast_pattern("$LValue = $Var#$Record{...$Fields...}.", Line), is_atom(Record),
            begin
                RValue =
                    case {is_var(Record, Line), get_field_var_list(Fields)} of
                        {false, {[], _}} ->
                            ast("$Var#$Record{...$Fields...}.", Line);
                        {false, {FieldList, RestFields}} ->
                            case lists:keyfind(Record, 1, RInfo) of
                                {_, FList} ->
                                    generate_rcase_for_static_record(Var, Record, FieldList, Line, FList, RestFields);
                                _ ->
                                    ast("$Var#$Record{...$Fields...}.", Line)
                            end;
                        {RecordVar, {[], RestFields}} ->
                            RestFieldsAtoms = [ erl_parse:normalise(FA) || {FA, _} <- RestFields],
                            Clauses =
                                [ ast("(@Name) -> $Var#@Name{...$Fields...}.", Line) ||  {Name, L} <- RInfo, (RestFieldsAtoms -- L) =:= [] ],
                            case Clauses of
                                [] -> throw(?mk_parse_error(Line, unknown_fields));
                                _ ->
                                    ast("case $RecordVar of ...$Clauses... end.", Line)
                            end;
                        {RecordVar, {FieldList, RestFields}} ->
                            RestFieldsAtoms = [ erl_parse:normalise(FA) || {FA, _} <- RestFields],
                            Clauses =
                                [
                                begin
                                    SubCase = generate_rcase_for_static_record(Var, RName, FieldList, Line, FList, RestFields),
                                    ast("(@RName) -> $SubCase.", Line)
                                end
                                ||
                                    {RName, FList} <- RInfo,
                                    (RestFieldsAtoms -- FList) =:= []
                                ],
                            case Clauses of
                                [] -> throw(?mk_parse_error(Line, unknown_fields));
                                _ ->
                                    ast("case $RecordVar of ...$Clauses... end.", Line)
                            end
                    end,
                ast("$LValue = $RValue.", Line)
            end
        },
        {
            ast_pattern("$LValue = #$Record{...$Fields...}.", Line), is_atom(Record),
            begin
                RValue =
                    case {is_var(Record, Line), get_field_var_list(Fields)} of
                        {false, {[], []}} ->
                            ast("#$Record{...$Fields...}.", Line);
                        {false, {FieldList, RestFields}} ->
                            case lists:keyfind(Record, 1, RInfo) of
                                {_, FList} ->
                                    generate_rcase_for_static_record(false, Record, FieldList, Line, FList, RestFields);
                                _ ->
                                    ast("#$Record{...$Fields...}.", Line)
                            end;
                        {RecordVar, {[], RestFields}} ->
                            RestFieldsAtoms = [ erl_parse:normalise(FA) || {FA, _} <- RestFields],
                            Clauses =
                                [ ast("(@Name) -> #@Name{...$Fields...}.", Line) ||  {Name, L} <- RInfo, (RestFieldsAtoms -- L) =:= [] ],
                            case Clauses of
                                [] -> throw(?mk_parse_error(Line, unknown_fields));
                                _ ->
                                    ast("case $RecordVar of ...$Clauses... end.", Line)
                            end;
                        {RecordVar, {FieldList, RestFields}} ->
                            RestFieldsAtoms = [ erl_parse:normalise(FA) || {FA, _} <- RestFields],
                            Clauses =
                                [
                                begin
                                    SubCase = generate_rcase_for_static_record(false, RName, FieldList, Line, FList, RestFields),
                                    ast("(@RName) -> $SubCase.", Line)
                                end
                                ||
                                    {RName, FList} <- RInfo,
                                    (RestFieldsAtoms -- FList) =:= []
                                ],
                            case Clauses of
                                [] -> throw(?mk_parse_error(Line, unknown_fields));
                                _ ->
                                    ast("case $RecordVar of ...$Clauses... end.", Line)
                            end
                    end,
                ast("$LValue = $RValue.", Line)
            end
        }
        ]).

get_field_var_list(Fields) ->
    lists:foldl(
        fun ({record_field, Line, El, Val}, {Acc1, Acc2}) ->
            case is_var(El, Line) of
                false -> {Acc1, [{El, Val} | Acc2]};
                Var -> {[{Var, Val} | Acc1], Acc2}
            end
        end,
        {[], []},
        Fields).

generate_rcase_for_static_record(Var, Record, FieldList, Line, FList, RestFields) ->
    Generator =
        fun (_FieldAtoms, [], Path, _Y) ->
                RR = [ {record_field, Line, pt_lib:abstract(RF, Line), RFL} || {RF, RFL} <- Path] ++ [ {record_field, Line, RF, RFL} || {RF, RFL} <- RestFields],
                case Var of
                    false -> ast("#$Record{...$RR...}.", Line);
                    _ -> ast("$Var#$Record{...$RR...}.", Line)
                end;
            (FieldAtoms, [{FieldVar, FieldVal} | FieldVarsTail], Path, Y) ->
                case FieldAtoms of
                    [] -> throw(?mk_parse_error(Line, {too_many_fields, Record}));
                    _ -> ok
                end,
                Cll = [ begin E = Y(lists:delete(F, FieldAtoms), FieldVarsTail, [{F, FieldVal} | Path], Y), ast(" (@F) -> $E. ", Line) end || F  <- FieldAtoms],
                ast("case $FieldVar of ...$Cll... end.", Line)
        end,
    Generator(FList -- [ erl_parse:normalise(FName) || {FName, _} <- RestFields], FieldList, [], Generator).



process_lvalue_records(AST, _RInfo) ->
    ?PATROL_ERROR("TODO: process lvalue records", []),
    AST.

is_var({atom, _, Atom}, Line) -> is_var(Atom, Line);
is_var(Atom, Line) when is_atom(Atom) ->
    List = atom_to_list(Atom),
    case re:run( List, "^{(?<P>.*)}$", [{capture, ['P'], list}] ) of
        {match, [Var]} -> {var, Line, pt_supp:string2atom(Var)};
        _ -> false
    end.


generate_megacase(Var, RVar, FVar, Line, RInfo) ->
    Clauses =
        [
            begin
                SubClauses = case Var of
                                false -> [ ast("(@FName) -> #@RName.@FName.", Line) || FName <- FieldList ];
                                _ ->     [ ast("(@FName) -> $Var#@RName.@FName.", Line) || FName <- FieldList ]
                             end,
                ast("(@RName) -> case $FVar of ...$SubClauses... end.", Line)
            end
        ||
            {RName, FieldList} <- RInfo
        ],
    ast("case $RVar of ...$Clauses... end.", Line).

format_error(Unknown) ->
    io_lib:format("Unknown error: ~p", [Unknown]).