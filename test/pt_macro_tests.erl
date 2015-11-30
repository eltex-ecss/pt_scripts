%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%%-------------------------------------------------------------------
-module(pt_macro_tests).

-include_lib("eunit/include/eunit.hrl").
-include("pt_macro.hrl").

pt_macro_func_name_with_arity_test() ->
    ?_test('pt_macro_func_name_with_arity_test/0' = ?FUNC).

pt_macro_func_name_without_arity_test() ->
    ?_test(pt_macro_func_name_without_arity_test = ?FUNC_NAME).

pt_macro_string_func_name_with_arity_test() ->
    ?_test("pt_macro_string_func_name_with_arity_test/0" = ?FUNC_STRING).

pt_macro_string_func_name_without_arity_test() ->
    ?_test(2 =:= pt_lib_tests:?FUNC_NAME(1)).
