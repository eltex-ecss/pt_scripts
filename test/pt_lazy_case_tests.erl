%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Nikita Roshchupkin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%% Created : Aug 2016
%%%-------------------------------------------------------------------
-module(pt_lazy_case_tests).

-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, pt_lazy_case}).

pt_lazy_case_test_() ->
    [
        ?_test(ok = lazy_case_test1()),
        ?_test(ok = lazy_case_test2()),
        ?_test(ok = lazy_case_test3()),
        ?_test(ok = lazy_case_test4()),
        ?_test(ok = lazy_case_test5()),
        ?_test(ok = lazy_case_test6())
    ].

lazy_case_test1() ->
    '@lazy_case',
    _Y = case {ok3, use} of
        {ok3, use} ->
            A1 = 1, A2 = 2,
            '@lazy_case',
            case ({A1, A2}) of
                {'VoiceMail', use_reader} ->
                    ?assert(false);
                _ ->
                    ok
            end;
        _ ->
            ?assert(false)
    end.

lazy_case_test2() ->
    A1 = 1, A2 = 2,
    '@lazy_case',
    case {A1, A2} of
        {"", _} ->
            ?assert(false);
        {_, ""}  ->
            ?assert(false);
        {_, _} ->
            ?assert(true)
    end,
    ok.

lazy_case_test3() ->
    A1 = 1, A2 = 2,
    '@lazy_case',
    case {A1, A2} of
        {line, 'UserStr'} ->
            ?assert(false);
        {message, 'UserStr'}  ->
            ?assert(false);
        {message, 'UserStrLine'} ->
            ?assert(false);
        {line, 'UserStrLine'} ->
            ?assert(false);
        {_, 'UserStr'} ->
            ?assert(false);
        {_, 'UserStrLine'} ->
            ?assert(false);
        _ ->
            ok
    end.

lazy_case_test4() ->
    A1 = 1, A2 = 2,
    '@lazy_case',
    X = case {A1, A2} of
        {undefined, undefined} ->
            ?assert(false);
        {undefined, all}  ->
            ?assert(false);
        {undefined, _} ->
            ?assert(false);
        {_, undefined} ->
            ?assert(false);
        {_, all} ->
            ?assert(false);
        {_, _} ->
            ok
    end,
    X.

lazy_case_test5() ->
    '@lazy_case',
    X =
        case {[], [], [], []} of
           {[], [], [], _V4} -> ok;
           {[], [], _V3,  _} -> ?assert(false);
           {[], _V2,  _,  _} -> ?assert(false);
           {_V1,  _,  _,  _} -> ?assert(false)
       end,
    X.

lazy_case_test6() ->
    '@lazy_case',
    X =
        case {[], []} of
           {[], _V} -> ok;
           {_V, _} -> ?assert(false)
        end,
    X.
