%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pt_macro_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/pt_macro.hrl").

pt_macro_test_() ->
    [
        ?_test('pt_macro_test_/0' = ?FUNC),
        ?_test(pt_macro_test_ = ?FUNC_NAME),
        ?_test("pt_macro_test_/0" = ?FUNC_STRING)
    ].