-module(pt_record_properties_tests).

-include("pt_record_properties.hrl").

-include_lib("eunit/include/eunit.hrl").

-include_records([test_1, test_2, test_3, test_4, terminal, account, codec]).

-record(test_1, {a, b, c}).
-record(test_2, {a = [] :: [#test_1{}], b = [] :: [#test_1{}], c, d}).
-record(test_3, {a = [] :: [#test_2{}], b = [] :: [#test_1{}], c, d}).
-record(test_4, {a = [] :: [#test_3{}], b, c, d}).

records_test() ->
    ?assertEqual([test_1, test_2, test_3, test_4], all_records()).

properties_test() ->
    ?assertEqual([{a, [{record, test_1}]},
                  {b, [{record, test_1}]},
                  {c, undefined},
                  {d, undefined}],
                 properties(test_2)),
    ?assertEqual([{a, [{record, test_2}]},
                  {b, [{record, test_1}]},
                  {c, undefined},
                  {d, undefined}],
                 properties(test_3)).
