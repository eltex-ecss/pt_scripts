-module(pt_guards_tests).

-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, pt_guard}).

pt_guards_test_() ->
    [
        ?_test(true_test1 = guard_test1(test_true, [test_true])),
        ?_test(false_test1 = guard_test1(test_false, [test_true])),
        ?_test(true_test2 = guard_test2()),
        ?_test(true_test3 = guard_test3(fun() -> true end)),
        ?_test(true_test4 = guard_test4([test_true])),
        ?_test(true_test5 = guard_test5([test_true]))
    ].

guard_test1(Test_atom, Test_list) when pt_guard(lists:member(Test_atom, Test_list)) ->
    true_test1;
guard_test1(_Test_atom, _Test_list) ->
    false_test1.

local_check() ->
    true.

guard_test2() when pt_guard(local_check()) ->
    true_test2;
guard_test2() ->
    false_test2.

guard_test3(Fun) when pt_guard(Fun()) ->
    true_test3;
guard_test3(_) ->
    false_test3.

guard_test4(Test_list) when is_list(Test_list) ->
    true_test4;
guard_test4(_) ->
    false_test4.

guard_test5(Test_list) when pt_guard(is_list(Test_list)) ->
    true_test5;
guard_test5(_) ->
    false_test5.