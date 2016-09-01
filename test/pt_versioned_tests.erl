%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @author platinumthinker <platinumthinker@gmail.com>
%%%-------------------------------------------------------------------
%%% @doc
%%% Format for build_info ->
%%%
%%% {build_date, {1472,716858,513054}},
%%% {build_author, "thinker"},
%%% {build_host, "think-eltex"},
%%% {commit, "0ca50a5f33b796c95b25f4c57dabade387fc3788"},
%%% {commit_author, "Andrey Teplyashin"},
%%% {commit_date, "Fri Aug 26 13:57:25 2016 +0700"}
%%% @end
%%%----------------------------------------------------------------------------

-module(pt_versioned_tests).
-include_lib("eunit/include/eunit.hrl").
-include("pt_versioned.hrl").

pt_version1_test() ->
    Info = ?MODULE:build_info(),
    ?assertMatch({{_, _, _}, _}, test_props(build_date, Info)),
    {_Val1, Info1} = test_props(build_date, Info),
    {_Val2, Info2} = test_props(build_author, Info1),
    {_Val3, Info3} = test_props(build_host, Info2),
    {_Val4, Info4} = test_props(commit, Info3),
    {_Val5, Info5} = test_props(commit_author, Info4),
    {_Val6, Info6} = test_props(commit_date, Info5),
    ?assertEqual(Info6, []).

pt_version2_test() ->
    ?assertMatch({_, _, _}, ?MODULE:build_info(build_date)),
    ?assertMatch(Val when is_list(Val), ?MODULE:build_info(build_author)),
    ?assertMatch(Val when is_list(Val), ?MODULE:build_info(build_host)),
    ?assertMatch(Val when is_list(Val), ?MODULE:build_info(commit)),
    ?assertMatch(Val when is_list(Val), ?MODULE:build_info(commit_author)),
    ?assertMatch(Val when is_list(Val), ?MODULE:build_info(commit_date)).

test_props(Key, List) ->
    ?assert(proplists:is_defined(Key, List)),
    Val = proplists:get_value(Key, List),
    {Val, proplists:delete(Key, List)}.
