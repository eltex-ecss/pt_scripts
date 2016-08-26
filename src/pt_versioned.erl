%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%%     add functions build_info/0 and build_info/1
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pt_versioned).

-export([parse_transform/2, add_build_info_fun/3, run_command/1]).

-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_lib/include/pt_patrol.hrl").
-include("pt_macro.hrl").
-include_lib("pt_lib/include/pt_types.hrl").

-patrol([{tty, error}]).


-define(GIT_LOG, "git log -1").
-define(DATE, "date -R").
-define(WHOAMI, "whoami").
-define(HOSTNAME, "hostname").
-define(COMMAND_TIMEOUT, 30000).


parse_transform(AST, Options) ->
    add_build_info_fun(AST, build_info, Options).


-spec add_build_info_fun(ast(), atom(), list()) -> ast().

add_build_info_fun(AST, FunName, Options) ->
    AST0 =
        case pt_lib:get_attribute_value(vsn, AST) of
            % Атрибут vsn не найден. Следовательно добавляем его.
            [] ->
                % Делаем vsn зависимым кроме AST так же и от версии application-а.
                AppFileVersion =
                    case pt_utils:get_app_options(vsn, Options) of
                        {ok, _AppFileVersion} ->
                            _AppFileVersion;
                        {error, {{error, no_app_file}, badarg}} ->
                            "";
                        {error, Reason} ->
                            ?PATROL_ERROR("Can't get app vsn: ~p", [Reason]),
                            ""
                    end,
                ASTToCalculateMD5 = lists:filter(fun({attribute,_,file,_}) -> false; (_) -> true end, AST),
                <<Vsn:128/unsigned-integer>> = erlang:md5(erlang:term_to_binary({AppFileVersion, ASTToCalculateMD5})),
                pt_lib:set_attribute(AST, vsn, [Vsn]);
            % Атрибут vsn найден. Не меняем его.
            _ ->
                AST
        end,
    Version1 =
        [{build_date, os:timestamp()}],

    Version2 =
        case run_command(?WHOAMI) of
            {ok, R4} -> [{build_author, strip(R4)} | Version1];
            timeout -> Version1
        end,

    Version3 =
        case run_command(?HOSTNAME) of
            {ok, R5} -> [{build_host, strip(R5)} | Version2];
            timeout -> Version2
        end,

    Version4 =
        case run_command(?GIT_LOG) of
            {ok, R2} -> %Version3 ++ [{git_log, R2}];
                case re:run(
                        R2,
                        "commit (?<COMMIT>\\S+)\\n(.*\\n)*Author: (?<AUTHOR>.+)\\s{1}\\<.*\\>\\n(.*\\n)*Date:\s+(?<DATE>.+)\\n.*",
                        [{capture, ['COMMIT', 'AUTHOR', 'DATE'], list}]) of

                    {match, [Commit, Author, Date]} ->
                        [{commit_date, Date}, {commit_author, Author}, {commit, Commit}  | Version3];
                    nomatch ->
                        Version3
                end;
            timeout -> Version3
        end,

    FinalVersion = lists:reverse(Version4),
    AST1 =
        try
            ASTTmp = pt_lib:add_function(AST0,
                ast("$FunName() -> @FinalVersion.", 0)),

            pt_lib:add_function(ASTTmp,
                ast("$FunName(Key) ->
                        case lists:keyfind(Key, 1, @FunName()) of
                            {Key, Value} -> Value;
                            false -> undefined
                        end.", 0))
        catch
            throw:{parse_error, {_, _, {fun_already_exists, _, _}}} -> AST0
        end,

    AST1.


-spec strip(string()) -> string().

strip(Str) ->
    Str1 = re:replace(Str,  "^(\\n|\\s)*", "", [unicode, {return, list}]),
    Str2 = re:replace(Str1, "(\\n|\\s)*$", "", [unicode, {return, list}]),
    Str2.



-spec run_command(string()) -> {ok, string()} | timeout.

run_command(Command) ->
    P = open_port({spawn, Command}, [exit_status]),
    read_port_data(P, []).



-spec read_port_data(port(), string()) -> {ok, string()} | timeout.

read_port_data(Port, Res) ->
    receive
        {Port, {data, Data}} -> read_port_data(Port, Res ++ Data);
        {Port, {exit_status, _}} -> {ok, Res};
        {Port, closed} -> {ok, Res}
    after
        ?COMMAND_TIMEOUT ->
            catch port_close(Port),
            ?PATROL_ERROR("Port ~p timeout", [Port]), timeout
    end.
