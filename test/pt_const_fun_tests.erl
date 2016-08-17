%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Nikita Roshchupkin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%% Created : Aug 2016
%%%-------------------------------------------------------------------
-module(pt_const_fun_tests).

-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, pt_const_fun}).

pt_const_fun_test_() ->
    [
        ?_test(true_test1 = pt_eval(const_fun_test1(test_true1))),
        ?_test(true_test1 = pt_eval(const_fun_test1([test_true2]))),
        ?_test(true_test1 = pt_eval(const_fun_test1(<<"test_true3">>))),
        ?_test(true_test1 = pt_eval(const_fun_test1("test_true3"))),
        ?_test(true_test1 = pt_eval(const_fun_test1({"test_true3"})))
    ].

const_fun_test1(_) ->
    true_test1.
