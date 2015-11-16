%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pt_records_tests).

-ifdef(TEST).
-include("pt_records.hrl").
-include("pt_recompilable.hrl").
-include_lib("eunit/include/eunit.hrl").

-type test() :: atom().

-record(ar, {a :: test(), b = 2 :: test(), c}).
-record(br, {d, e}).
-record(cr, {f :: test(), g = 2, h, i, j, k}).
-record(dr, {i, j}).

parse_transform_test_() ->
    [
        ?_test(
        begin
            RName = br,
            R = #br{e = 1},
            1 = R#'{RName}'.e
        end),
        ?_test(
        begin
            FName = b,
            R = #ar{b = 1},
            1 = R#ar.'{FName}'
        end),
        ?_test(
        begin
            RName = g(br),
            FName = g(e),
            R = #br{e = 1},
            1 = R#'{RName}'.'{FName}'
        end),
        ?_test(
        begin
            RName = br,
            #br.e =:= #'{RName}'.e
        end),
        ?_test(
        begin
            FName = b,
            #ar.b =:= #ar.'{FName}'
        end),
        ?_test(
        begin
            RName = g(br),
            FName = g(e),
            #br.e =:= #'{RName}'.'{FName}'
        end),
        ?_test(
        begin
            RName = g(br),
            FName = g(e),
            R = #br{e = 1},
            test(R#'{RName}'.'{FName}')
        end),
        ?_test(
        begin
            FName = d,
            R = #br{},
            {br, 1, _} = R#br{'{FName}' = 1}
        end),
        ?_test(
        begin
            FName = d,
            R = #br{},
            {br, 1, 2} = R#br{e = 2, '{FName}' = 1}
        end),
        ?_test(
        begin
            FName = g(b),
            FName2 = g(c),
            R = #ar{},
            {ar, 1, 2, 3} = R#ar{a = 1, '{FName}' = 2, '{FName2}' = 3}
        end),
        ?_test(
        begin
            FName = g(f),
            FName2 = g(h),
            FName3 = g(j),
            R = #cr{},
            {cr, 1, 2, 3, 4, 5, undefined} = R#cr{i = 4, '{FName}' = 1, '{FName3}' = 5, '{FName2}' = 3}
        end),

        ?_test(
        begin
            FName = d,
            {br, 1, undefined} = #br{'{FName}' = 1}
        end),
        ?_test(
        begin
            FName = d,
            {br, 1, 2} = #br{e = 2, '{FName}' = 1}
        end),
        ?_test(
        begin
            FName = g(b),
            FName2 = g(c),
            {ar, 1, 2, 3} = #ar{a = 1, '{FName}' = 2, '{FName2}' = 3}
        end),
        ?_test(
        begin
            FName = g(f),
            FName2 = g(h),
            FName3 = g(j),
            {cr, 1, 2, 3, 4, 5, undefined} = #cr{i = 4, '{FName}' = 1, '{FName3}' = 5, '{FName2}' = 3}
        end),

        ?_test(
        begin
            FName = g(g),
            RName = g(cr),
            R = g(#cr{}),
            {cr, undefined, 1, undefined, undefined, undefined, undefined} = R#'{RName}'{'{FName}' = 1}
        end),
        ?_test(
        begin
            FName = g(g),
            RName = g(cr),
            R = g(#cr{}),
            {cr, undefined, 1, undefined, 3, undefined, undefined} = R#'{RName}'{'{FName}' = 1, i = 3}
        end),
        ?_test(
        begin
            RName = g(cr),
            R = g(#cr{}),
            {cr, undefined, 2, undefined, 3, undefined, undefined} = R#'{RName}'{i = 3}
        end),

        ?_test(
        begin
            FName = g(g),
            RName = g(cr),
            {cr, undefined, 1, undefined, undefined, undefined, undefined} = #'{RName}'{'{FName}' = 1}
        end),
        ?_test(
        begin
            FName = g(g),
            RName = g(cr),
            {cr, undefined, 1, undefined, 3, undefined, undefined} = #'{RName}'{'{FName}' = 1, i = 3}
        end),
        ?_test(
        begin
            RName = cr,
            {cr, undefined, 2, undefined, 3, undefined, undefined} = #'{RName}'{i = 3}
        end)

        %?_test(
        %begin
        %    FName = d,
        %    R = #br{d = 2, e = 3},
        %    #br{'{FName}' = AA, e = BB} = R,
        %    AA == 2,
        %    BB == 3
        %end)
        %?_test(
        %begin
        %    RName = br,
        %    FName = d,
        %    R = #br{d = 1, e = 2},
        %    #'{RName}'{e = A, '{FName}' = B} = R,
        %    a = 1, B = 2
        %end)
    ].

test (A) -> A.

g(A) -> A.
-endif.