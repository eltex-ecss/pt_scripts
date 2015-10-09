%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%     Add function get_info/0 with base git repo information
%%% @end
%%%-------------------------------------------------------------------
-module(pt_build_info).

-export([parse_transform/2]).

-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_lib/include/pt_patrol.hrl").
-include("pt_macro.hrl").
-include_lib("pt_lib/include/pt_types.hrl").

-patrol([
         {tty, error}
        ]).

-define(GIT_STATUS, "git status").


parse_transform(AST, Options) ->
    AST1 = pt_versioned:add_build_info_fun(AST, get_info, Options),

    AST2 =
        case pt_versioned:run_command(?GIT_STATUS) of
            {ok, R} ->
                [ast_pattern("$FName() -> $List.", Line) = F] = pt_lib:match(AST1, ast_pattern("get_info/0 [...$_...].")),
                NewList = pt_lib:list_concat(List, ast("[{git_status, @R}].", Line)),
                ASTTmp = pt_lib:replace(AST1, F, ast("$FName() -> $NewList.", Line)),
                pt_lib:add_function(ASTTmp,
                    ast("get_info_str() ->
                        Info = get_info(),
                        Str = lists:foldl(
                            fun ({Key, Value}, Acc) ->
                                Acc ++ io_lib:format(\"~w: ~s~n\", [Key, Value])
                            end,
                            [],
                            Info),
                        lists:flatten(Str).", 1));
            timeout -> AST1
        end,

    AST2.