%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% Allow you to recompile you module at runtime with any parse transformations
%%% Usage:
%%%
%%% -include("pt_scripts/include/pt_recompilable.hrl").
%%%
%%% self_recompile() ->
%%%    pt_recompilable:recompile_cur_module(?MODULE,
%%%            fun (AST, Options) ->                   % AST - ast of curently loaded code of module ?MODULE
%%%            %... do whatever you want here
%%%            end, []).
%%%
%%% recompile_orig() ->
%%%    pt_recompilable:recompile_orig_module(?MODULE,
%%%            fun (AST, Options) ->                   % AST - ast of origin code of module ?MODULE (that was generated at project compilation)
%%%            %... do whatever you want here
%%%            end, []).
%%%
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pt_recompilable).

-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_lib/include/pt_patrol.hrl").
-include("pt_macro.hrl").
-include_lib("pt_lib/include/pt_types.hrl").


-patrol([{tty, error}]).

-export([
         add_current_ast_fun/2,
         add_orig_ast_fun/2,
         parse_transform/2,
         recompile_cur_module/2,
         recompile_cur_module/3,
         recompile_orig_module/2,
         recompile_orig_module/3,
         switch_off_function/2,
         switch_off_function_clause/4,
         switch_on_function/2,
         switch_on_function_clause/4
        ]).

-spec parse_transform(ast(), term()) -> ast().

parse_transform(AST, _Options) ->
    AST1 = add_orig_ast_fun(AST, AST),
    AST2 = add_current_ast_fun(AST1, AST),
    AST2.


-spec add_orig_ast_fun(ast(), ast()) -> ast().

add_orig_ast_fun(AST, OrigAST) ->
    %for debug only:
    AST1 = pt_lib:add_function(AST, ast("print_orig_source() -> io:format(\"~s\", [pt_lib:ast2str(get_orig_ast())]).", 0)),
    pt_lib:add_function(AST1,  ast("get_orig_ast() -> '@OrigAST'.", 0)).

-spec add_current_ast_fun(ast(), ast()) -> ast().

add_current_ast_fun(AST, CurAST) ->
    AST1 = pt_lib:add_function(AST, ast("print_current_source() -> io:format(\"~s\", [pt_lib:ast2str(get_current_ast())]).", 0)),
    pt_lib:add_function(AST1, ast("get_current_ast() -> '@CurAST'.", 0)).

-spec switch_on_function(atom(), {atom(), arity()} | [{atom(), arity()}]) -> ok | {error, term()}.

switch_on_function(Module, {FunctionName, Arity}) ->
    switch_on_function(Module, [{FunctionName, Arity}]);
switch_on_function(Module, Functions) when is_list(Functions) ->

    FunctionsWithCode =
        try
            OrigAst =
                case erlang:function_exported(Module, get_orig_ast, 0) of
                    true -> Module:get_orig_ast();
                    _ -> throw({cant_get_ast, Module})
                end,
            lists:foldl(
                fun ({FunctionName, Arity}, Acc) ->
                    case pt_lib:get_function_ast(OrigAst, FunctionName, Arity) of
                        {ok, FunAst} -> [{FunctionName, Arity, FunAst} | Acc];
                        Err -> throw({cant_find_function, {FunctionName, Arity}, Module, Err})
                    end
                end,
                [],
                Functions)
        catch
            throw:E -> {error, E};
            _:E -> {error, {E, erlang:get_stacktrace()}}
        end,

    recompile_cur_module(Module,
        fun (AST, _) ->
            lists:foldl(
                fun({FunctionName, Arity, Code}, AccAST) ->
                    pt_lib:replace(AccAST, {function, _, FunctionName, Arity, _}, Code)
                end,
                AST,
                FunctionsWithCode
            )
        end
    ).

-spec switch_off_function(atom(), {atom(), arity()} | [{atom(), arity()}]) -> ok | {error, term()}.

switch_off_function(Module, {FunctionName, Arity}) ->
    switch_off_function(Module, [{FunctionName, Arity}]);
switch_off_function(Module, Functions) when is_list(Functions) ->
    recompile_cur_module(Module,
        fun (AST, _) ->
            lists:foldl(
                fun({FunctionName, Arity}, AccAST) ->
                    Params = lists:duplicate(Arity, ast("_.", 0)),
                    pt_lib:replace(AccAST, {function, Line, FunctionName, Arity, _}, ast("$FunctionName(...$Params...) -> ok.", Line))
                end,
                AST,
                Functions
            )
        end).

-spec switch_on_function_clause(atom(), atom(), arity(), [{ast(), ast()}] | {ast(), ast()}) -> ok | {error, term()}.

switch_on_function_clause(Module, FunctionName, Arity, {Params, Guards}) ->
    switch_on_function_clause(Module, FunctionName, Arity, [{Params, Guards}]);
switch_on_function_clause(Module, FunctionName, Arity, ParamsAndGuards) when is_list(ParamsAndGuards) ->
    ParamsGuardsAndCode =
        try
            OrigAst =
                case erlang:function_exported(Module, get_orig_ast, 0) of
                    true -> Module:get_orig_ast();
                    _ -> throw({cant_get_ast, {FunctionName, Arity}, Module})
                end,
            lists:foldl(
                fun ({Params, Guards}, Acc) ->
                    case pt_lib:get_function_ast(OrigAst, FunctionName, Arity) of
                        {ok, {function, _, FunctionName, Arity, Clauses}} ->
                            ClauseCode = lists:foldl(
                                fun ({clause, _, CParams, CGuards, CCode}, AccCode) ->
                                    A = pt_supp:insert_lines(CParams, 0),
                                    B = pt_supp:insert_lines(Params, 0),
                                    C = pt_supp:insert_lines(CGuards, 0),
                                    D = pt_supp:insert_lines(Guards, 0),
                                    if (A =:= B) and (C =:= D) -> CCode;
                                       true -> AccCode
                                    end
                                end,
                                [],
                                Clauses
                            ),
                            case ClauseCode of
                                [] -> throw({cant_find_clause, {FunctionName, Arity}, Module});
                                _ -> [{Params, Guards, ClauseCode} | Acc]
                            end;
                        Err -> throw({cant_find_function, {FunctionName, Arity}, Module, Err})
                    end
                end,
                [],
                ParamsAndGuards
            )
    catch
        throw:E -> {error, E};
        _:E -> {error, {E, erlang:get_stacktrace()}}
    end,

    recompile_cur_module(Module,
        fun (AST, _) ->
            lists:foldl(
                fun ({Params, Guards, Code}, AccAST) ->
                    pt_supp:replace_function_clause_code(AccAST, FunctionName, Arity, Params, Guards, Code)
                end,
                AST,
                ParamsGuardsAndCode
            )
        end).

-spec switch_off_function_clause(module(), atom(), arity(),
                                 [{ast(), ast()}] | {ast(), ast()}) ->
    ok | {error, term()}.

switch_off_function_clause(Module, FunctionName, Arity, {Params, Guards}) ->
    switch_off_function_clause(Module, FunctionName, Arity, [{Params, Guards}]);
switch_off_function_clause(Module, FunctionName, Arity, ParamsAndGuards) when is_list(ParamsAndGuards) ->
    recompile_cur_module(Module,
        fun (AST, _) ->
            lists:foldl(
                fun({Params, Guards}, AccAST) ->
                    pt_supp:replace_function_clause_code(AccAST, FunctionName,
                                        Arity, Params, Guards, [{atom, 0, ok}])
                end, AST, ParamsAndGuards)
        end).

-spec recompile_orig_module(atom(), fun ( (ast(), term()) -> ast() )) -> ok | {error, term()}.

recompile_orig_module(ModuleName, PT_Fun) ->
    recompile_orig_module(ModuleName, PT_Fun, []).

-spec recompile_orig_module(atom(), fun ( (ast(), term()) -> ast() ), term()) -> ok | {error, term()}.

recompile_orig_module(ModuleName, PT_Fun, Options) ->
    case (catch ModuleName:get_orig_ast()) of
        AST when is_list(AST) ->
            try
                AST1 = PT_Fun(AST, Options),
                AST2 = add_orig_ast_fun(AST1, AST),
                AST3 = add_current_ast_fun(AST2, AST1),
                pt_lib:compile_and_load(AST3)
            catch
                throw:E -> {error, E};
                _:E -> {error, {E, erlang:get_stacktrace()}}
            end;
        Err ->
            {error, {cant_get_ast, ModuleName, Err}}
    end.

-spec recompile_cur_module(module(), fun ( (ast(), term()) -> ast() )) ->
    ok | {error, term()}.

recompile_cur_module(ModuleName, PT_Fun) ->
    recompile_cur_module(ModuleName, PT_Fun, []).

-spec recompile_cur_module(module(), fun ( (ast(), term()) -> ast() ), term()) ->
    ok | {error, _Reason}.

recompile_cur_module(ModuleName, PT_Fun, Options) ->
    case (catch ModuleName:get_current_ast()) of
        AST when is_list(AST) ->
            try
                AST1 = PT_Fun(AST, Options),
                AST2 = add_orig_ast_fun(AST1, ModuleName:get_orig_ast()),
                AST3 = add_current_ast_fun(AST2, AST1),
                pt_lib:compile_and_load(AST3)
            catch
                throw:E -> {error, E};
                _:E -> {error, {E, erlang:get_stacktrace()}}
            end;
        Err ->
            {error, {cant_get_ast, ModuleName, Err}}
    end.