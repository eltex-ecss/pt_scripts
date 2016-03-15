%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Nikita Roshchupkin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%% Created : Mar 2016
%%%-------------------------------------------------------------------
-module(pt_fun_guards_tests).

-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, pt_fun_guards}).
-compile({parse_transform, pt_recompilable}).

pt_fun_guards_test_() ->
    [
        ?_test(true_test1 = fun_guards_test1(test_true1)),
        ?_test(true_test2 = fun_guards_test2(test_true2)),
        ?_test(true_test3 = fun_guards_test3(test_true3)),
        ?_test(true_test4 = fun_guards_test4(test_true4, [test_true4]))
    ].

fun_guards_test1(Test_var) when lists:member(Test_var, [test_true1]) ->
    true_test1;
fun_guards_test1(_Test_var) ->
    false_test1.

fun_guards_test2(Test_var) ->
    case Test_var of
        Var when lists:member(Var, [test_true2]) ->
            true_test2;
        _ ->
            false_test2
    end.

fun_guards_test3(Test_var) when lists:member(Test_var, [test_true3]) ->
    case Test_var of
        Var when lists:member(Var, [test_true3]) ->
            true_test3;
        _ ->
            false_test3
    end;
fun_guards_test3(_Test_var) ->
    false_test3.

fun_guards_test4(Test_var, Test_list) when lists:member(Test_var, Test_list) ->
    case Test_var of
        Var when lists:member(Var, [test_true4]) ->
            true_test4;
        _ ->
            false_test4
    end;
fun_guards_test4(_Test_var, _Test_list) ->
    false_test4.