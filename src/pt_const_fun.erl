%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Nikita Roshchupkin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%     Replaces the function with constant data on the results of their work
%%%     Example:
%%%     module(test).
%%%     -export([test/0]).
%%%     -include_lib("chronica/include/chronica.hrl").
%%%     -compile({parse_transform, pt_const_fun}).
%%%     -pt_eval_timeout(0). %Option set the maximum waiting time when compiling
%%%
%%%     test_const(alias) ->
%%%         [asd, sd, d];
%%%     test() ->
%%%         A = pt_eval(test_const(alias)). => A = [asd, sd, d]
%%%
%%% pt_eval is a sign of a replacement
%%% @end
%%% Created : Aug 2016
%%%-------------------------------------------------------------------
-module(pt_const_fun).

-export([parse_transform/2, format_error/1]).

-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_lib/include/pt_error_macro.hrl").
-include_lib("pt_lib/include/pt_patrol.hrl").

-patrol([{tty, error}]).

parse_transform(AST, _) ->
    ListAST = pt_lib:match(AST, ast_pattern("pt_eval('$_').", _)),
    Timeout = case pt_lib:get_attribute_value(pt_eval_timeout, AST) of
        [Timeout1] ->
            Timeout1;
        _ ->
            1000
    end,
    NewListAST = replace_const_fun(ListAST, Timeout),
    % Здесь, можно было бы изменить имя модуля
    Module = pt_lib:get_module_name(AST),
    NameCall =  list_to_atom("calc" ++ erlang:atom_to_list(Module)),
    F1 = ast("$NameCall() -> L = $NewListAST, [F() || F <- L].", 0),
    AST2 = pt_lib:add_function(AST, F1),
    {AST3, _} = pt_lib:replace_fold(AST2, [
        {
            {ast_pattern("pt_eval('$String').", _Line), Acc},
            begin
                {ast("$String.", _Line), Acc}
            end
        }], []),
    try
        case catch pt_lib:compile_and_load(AST3) of
            ok ->
                Data = Module:NameCall(),
                {AST4, _} = create_new_ast(Data, AST),
                true = code:delete(Module),
                case compile:forms(AST4, [return_warnings]) of
                    {ok, _, _, [{_, Warning}]} ->
                        delete_unused_functions(Warning, AST4);
                    _ ->
                        AST4
                end;
            _Er ->
                throw(?mk_parse_error(0, {dynamic_data, {error, compile_and_load}})),
                AST
        end
    catch
        throw:{error, {pt_lib, _, _, _}} ->
            {ok, Cwd} = file:get_cwd(),
            File = pt_lib:get_file_name(AST),
            FullFile = filename:join(Cwd, File),
            io_lib:format("~ts: Warning: can not compile", [FullFile]),
            AST
    end.

delete_unused_functions([{_, _, {unused_function, {Name, CountArg}}} | TailWarning], AST) ->
    NewAST = delete_unused_function(AST, {Name, CountArg}, []),
    delete_unused_functions(TailWarning, NewAST);
delete_unused_functions([_ | TailWarning], AST) ->
    delete_unused_functions(TailWarning, AST);
delete_unused_functions(_, AST) ->
    AST.

delete_unused_function([{function, _, Name, CountArg, _} | TailAST], {Name, CountArg}, Acc) ->
    delete_unused_function(TailAST, {Name, CountArg}, Acc);
delete_unused_function([HeadAST | TailAST], {Name, CountArg}, Acc) ->
    delete_unused_function(TailAST, {Name, CountArg}, [HeadAST | Acc]);
delete_unused_function(_, _, Acc) ->
    lists:reverse(Acc).

replace_const_fun([HeadAST | TailAST], Timeout) ->
    {NewHeadAST, _} = pt_lib:replace_fold(HeadAST, [
        {
            {ast_pattern("pt_eval('$String').", _), Acc},
            begin
                {create_new_fun(String, Timeout), Acc}
            end
        }], []),
    {cons, 0, NewHeadAST, replace_const_fun(TailAST, Timeout)};
replace_const_fun(_, _) ->
    {nil, 0}.

create_new_ast(Data, AST) ->
    pt_lib:replace_fold(AST, [
        {
            {ast_pattern("pt_eval('$String').", Line), [Head | Tail]},
            begin
                case Head of
                    {ok, NewData} ->
                        {ast("@NewData.", Line), Tail};
                    _ ->
                        throw(?mk_parse_error(Line, {dynamic_data, Head})),
                        {ast("$String.", Line), Tail}
                end
            end
        }], Data).

create_new_fun(String, Timeout) ->
    CallFunction = ast("fun() ->
        try
            Data = $String,
            Self ! {{ok, Data}, Ref}
        catch
            Exception:Reason ->
                Self ! {{error, Exception, Reason}, Ref}
        end
    end.", 0),

    ast("fun() ->
        Self = self(),
        Ref = make_ref(),
        spawn($CallFunction),
        receive
            {Result, Ref} ->
                Result
            after
                @Timeout ->
                    {error, timeout}
            end
    end.", 0).

format_error({dynamic_data, Error}) ->
    case Error of
        {error, TypeError, NameError} ->
            io_lib:format("~p: occurred during compilation pt_eval ~p", [TypeError, NameError]);
        {error, NameError} ->
            io_lib:format("~p: occurred during compilation pt_eval ~p", [error, NameError]);
        _ ->
            io_lib:format("~p: can not compile pt_eval", [warning])
    end;
format_error(Unknown) ->
    io_lib:format("Unknown error: ~p", [Unknown]).
