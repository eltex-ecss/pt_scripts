%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Konstantin '$ky' Sivakov
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%% string:match(Input, Match, Match, ..., Match) -> ok.
%%%
%%% PEG:
%%%
%%% Main    :: Token+
%%% Token   :: Match / Exp
%%% Match   :: Var = Exp
%%% Var     :: [A-Z].*
%%% Exp     :: Seq / Opt / Sel / Pattern
%%% Seq     :: \{Token+\}
%%% Opt     :: \[Token+\]
%%% Sel     :: \{\[Token+\]\}
%%% Pattern :: Type / String
%%% Type    :: [a-z]+
%%% String  :: \".+\"
%%%
%%% string:match(Input, Name = string, Value = any) -> ok.
%%%
%%% string:match(Input, FullName = {Name = string, Secod = string}, Profile = string, Switch = ["/d+/w", {["after", "before"], string}]) -> ok.
%%%
%%% @end
%%% Created :  2 Jul 2010
%%%-------------------------------------------------------------------
-module(pt_str_parser).

-include_lib("pt_lib/include/pt_error_macro.hrl").
-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_lib/include/pt_patrol.hrl").

-export([
         parse_transform/2,
         format_error/1
        ]).

%% API
-export([
        ]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% Options
%%--------------------------------------------------------------------
-define(PT_STR_PARSER, 'str_parser').

%%--------------------------------------------------------------------
%% Token Types
%%--------------------------------------------------------------------
-define(PT_STR_PEG_SEQ, 'peg-seq').
-define(PT_STR_PEG_OPT, 'peg-opt').
-define(PT_STR_PEG_MTC, 'peg-mtc').

-record(?PT_STR_PEG_SEQ, {exp :: string()}).
-record(?PT_STR_PEG_OPT, {exp :: string()}).
-record(?PT_STR_PEG_MTC, {exp :: string(), var :: string()}).

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------
-define(PT_STR_PARSER_STR, string).
-define(PT_STR_PARSER_INT, integer).
-define(PT_STR_PARSER_NEG_INT, neg_integer).
-define(PT_STR_PARSER_NON_NEG_INT, non_neg_integer).
-define(PT_STR_PARSER_NUM, number).
-define(PT_STR_PARSER_FLOAT, float).
-define(PT_STR_PARSER_BOOL, boolean).
-define(PT_STR_PARSER_ANY, any).
-define(PT_STR_PARSER_DELIM, delim).
-define(PT_STR_PARSER_OPTDELIM, optdelim).
-define(PT_STR_PARSER_HEAD, head).
-define(PT_STR_PARSER_END, 'end').

%%--------------------------------------------------------------------
%% Patterns
%%--------------------------------------------------------------------
-define(PT_STR_PARSER_STR_PAT, "((\"([^\"]*)\")|(\'([^\']*)\')|(\\S+))").
-define(PT_STR_PARSER_INT_PAT, "[+-]?\\d+").
-define(PT_STR_PARSER_NEG_INT_PAT, "-\\d+").
-define(PT_STR_PARSER_NON_NEG_INT_PAT, "[+]?\\d+").
-define(PT_STR_PARSER_FLOAT_PAT, "[+-]?\\d+(\\.\\d+)?").
-define(PT_STR_PARSER_NUM_PAT, ?PT_STR_PARSER_FLOAT_PAT).
-define(PT_STR_PARSER_BOOL_PAT, "((?i)(true|false)|(yes|no)|(on|off)|(1|0)|(y|n)|(enable|disable))").
-define(PT_STR_PARSER_ANY_PAT, ".*").
-define(PT_STR_PARSER_DELIM_PAT, "\\s+").
-define(PT_STR_PARSER_OPTDELIM_PAT, "\\s*").
-define(PT_STR_PARSER_HEAD_PAT, "^\\s*").
-define(PT_STR_PARSER_END_PAT, "$"). %"

-define(PT_STR_PARSER_DEF, [
                            {?PT_STR_PARSER_STR, ?PT_STR_PARSER_STR_PAT},
                            {?PT_STR_PARSER_INT, ?PT_STR_PARSER_INT_PAT},
                            {?PT_STR_PARSER_NEG_INT, ?PT_STR_PARSER_NEG_INT_PAT},
                            {?PT_STR_PARSER_NON_NEG_INT, ?PT_STR_PARSER_NON_NEG_INT_PAT},
                            {?PT_STR_PARSER_FLOAT, ?PT_STR_PARSER_FLOAT_PAT},
                            {?PT_STR_PARSER_NUM, ?PT_STR_PARSER_NUM_PAT},
                            {?PT_STR_PARSER_BOOL, ?PT_STR_PARSER_BOOL_PAT},
                            {?PT_STR_PARSER_ANY, ?PT_STR_PARSER_ANY_PAT},
                            {?PT_STR_PARSER_DELIM, ?PT_STR_PARSER_DELIM_PAT},
                            {?PT_STR_PARSER_OPTDELIM, ?PT_STR_PARSER_OPTDELIM_PAT},
                            {?PT_STR_PARSER_HEAD, ?PT_STR_PARSER_HEAD_PAT},
                            {?PT_STR_PARSER_END, ?PT_STR_PARSER_END_PAT}
                           ]).

%%--------------------------------------------------------------------
%% Parser state
%%--------------------------------------------------------------------
-define(PT_STR_PARSER_VAR, 'pt-str-parser-var').
-define(PT_STR_PARSER_TYPE, 'pt-str-parser-type').

-type ?PT_STR_PARSER_VAR() :: {term(), atom()}.
-type ?PT_STR_PARSER_TYPE() :: {atom(), string()}.

-record(?PT_STR_PARSER, {exp = [] :: [string()],
                         bind = [] :: [?PT_STR_PARSER_VAR()],
                         count  = 0 :: non_neg_integer(),
                         types :: [?PT_STR_PARSER_TYPE()],
                         line = 0}).


%%--------------------------------------------------------------------
%% Errors
%%--------------------------------------------------------------------
-define(PT_STR_PARSER_ERR_INVAL_EXP, 'pt-str-parser-inval-exp').
-define(PT_STR_PARSER_ERR_INVAL_TYPE_EXP, 'pt-str-parser-inval-type-exp').
-define(PT_STR_PARSER_ERR_BAD_EXP, 'pt-str-parser-bad-exp').
-define(PT_STR_PARSER_ERR_BAD_TYPE, 'pt-str-parser-bad-type').
-define(PT_STR_PARSER_ERR_NO_MATCH_SUBJ, 'pt-str-parser-no-match-subj').
-define(PT_STR_PARSER_ERR_BAD_ARG, 'pt-str-parser-bad-arg').
-define(PT_STR_PARSER_ERR_UNKNOWN_TYPE, 'pt-str-parser-unknown-type').
-define(PT_STR_PARSER_ERR_BAD_MATCH_EXP, 'pt-str-parser-bad-match-exp').
-define(PT_STR_PARSER_ERR_BAD_VAR, 'pt-str-parser-bad-var').
-define(PT_STR_PARSER_ERR_INVAL_CFG, 'pt-str-parser-inval-cfg').

-record(?PT_STR_PARSER_ERR_INVAL_EXP, {exp :: string(), error :: term()}).
-record(?PT_STR_PARSER_ERR_INVAL_TYPE_EXP, {type :: atom(), inval :: #?PT_STR_PARSER_ERR_INVAL_EXP{}}).
-record(?PT_STR_PARSER_ERR_BAD_EXP, {exp :: string(), msg :: string()}).
-record(?PT_STR_PARSER_ERR_BAD_TYPE, {type :: term()}).
-record(?PT_STR_PARSER_ERR_NO_MATCH_SUBJ, {}).
-record(?PT_STR_PARSER_ERR_BAD_ARG, {arg :: term(), error :: term()}).
-record(?PT_STR_PARSER_ERR_UNKNOWN_TYPE, {type :: atom()}).
-record(?PT_STR_PARSER_ERR_BAD_MATCH_EXP, {exp :: term()}).
-record(?PT_STR_PARSER_ERR_BAD_VAR, {var :: term()}).
-record(?PT_STR_PARSER_ERR_INVAL_CFG, {cfg :: term()}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


parse_transform(AST, _Options) ->
    Params =
        parseParams(?PT_STR_PARSER_DEF,
                    pt_lib:get_attribute_value(?PT_STR_PARSER, AST)),

    build(AST, Params).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-spec format_error(term()) ->
    string().

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------

format_error(#?PT_STR_PARSER_ERR_INVAL_CFG{cfg = Cfg}) ->
    io_lib:format("Invalid parser configuration: ~p. Parser configuration should be formed like [{Type :: atom(), Exp :: string()}]", [Cfg]);

format_error(#?PT_STR_PARSER_ERR_BAD_MATCH_EXP{exp = Exp}) ->
    io_lib:format("Bad formed match expression: ~p. Match expression should be formed like Pattern = <expression>.", [Exp]);

format_error(#?PT_STR_PARSER_ERR_BAD_VAR{var = Var}) ->
    io_lib:format("Bad variable: ~p", [Var]);

format_error(#?PT_STR_PARSER_ERR_BAD_ARG{arg = Arg, error = Error}) ->
    io_lib:format("Bad expression argument: ~p. Error: ~p", [Arg, Error]);

format_error(#?PT_STR_PARSER_ERR_UNKNOWN_TYPE{type = Type}) ->
    io_lib:format("Unknown type: ~w", [Type]);

format_error(#?PT_STR_PARSER_ERR_NO_MATCH_SUBJ{}) ->
    io_lib:format("There aint match subject", []);

format_error(#?PT_STR_PARSER_ERR_INVAL_EXP{exp = Exp, error = Error}) ->
    io_lib:format("Invalid expression: ~s. Error: ~p", [Exp, Error]);

format_error(#?PT_STR_PARSER_ERR_INVAL_TYPE_EXP{type = Type,
                                             inval = #?PT_STR_PARSER_ERR_INVAL_EXP{exp = Exp,
                                                                                error = Error}}) ->
    io_lib:format("Invalid type expression for type ~w. Expression: ~s. Error: ~p", [Type, Exp, Error]);

format_error(#?PT_STR_PARSER_ERR_BAD_EXP{exp = Exp, msg = Msg}) ->
    io_lib:format("Bad formed expression: ~s. ~s", [Msg, Exp]);

format_error(#?PT_STR_PARSER_ERR_BAD_TYPE{type = Type}) ->
    io_lib:format("Bad formed type name: ~p. Type name should be atom", [Type]);

format_error(Unknown) ->
    io_lib:format("Unknown error: ~p", [Unknown]).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%%%===================================================================
%%% Internal functions
%%%===================================================================


parseParams(Params, []) ->
    Params;

parseParams(Params, [_ | _] = Def) ->
    lists:foldl(fun paramParser/2, Params, Def);

parseParams(_, _Other) ->
    throw(?mk_parse_error(0, '')). %% TODO
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


paramParser({Item, {group, GroupItems}}, Params) when is_atom(Item),
                                                      is_list(GroupItems) ->
    PrepareGroupItemFun =
        fun(GItem) when is_atom(GItem) ->
                erlang:atom_to_list(GItem);
           (GItem) when is_list(GItem) ->
                GItem;
           (GItem) ->
                throw(?mk_parse_error(0, #?PT_STR_PARSER_ERR_INVAL_TYPE_EXP{type = GItem,
                                                                            inval = #?PT_STR_PARSER_ERR_INVAL_EXP{exp = "",
                                                                                                                  error = invalid_group_item}}))
        end,
    GoupItemsStrs = lists:map(PrepareGroupItemFun, GroupItems),
    Exp = "(" ++ string:join(GoupItemsStrs, "|") ++ ")",
    paramParser({Item, Exp}, Params);

paramParser({Item, Exp} = Param, Params) when is_atom(Item),
                                              is_list(Exp) ->
    case re:compile(Exp) of
        {ok, _} ->
            lists:keystore(Item, 1, Params, Param);

        {error, Error} ->
            throw(?mk_parse_error(0, #?PT_STR_PARSER_ERR_INVAL_TYPE_EXP{type = Item,
                                                                        inval = #?PT_STR_PARSER_ERR_INVAL_EXP{exp = Exp,
                                                                                                              error = Error}}))
    end;

paramParser(_Param, _) ->
    throw(?mk_parse_error(0, '')). %% TODO
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


build(AST, Params) ->
    pt_lib:replace(AST,
                   ast_pattern("string:match(...$MatchList...).", Line),
                   parseMatchList(MatchList, Params, Line)).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


parseMatchList([], _, Line) ->
    throw(?mk_parse_error(Line, #?PT_STR_PARSER_ERR_NO_MATCH_SUBJ{}));


parseMatchList([_], _, Line) ->
    ast("ok.", Line);


parseMatchList([Input | Matches], Types, Line) ->
    Parser =
        lists:foldl(fun parseMatch/2, #?PT_STR_PARSER{types = Types, line = Line}, Matches),

    Mp =
        buildRegex(Parser),

    {Vars, Names} = lists:unzip(lists:reverse(Parser#?PT_STR_PARSER.bind)),

    Match =
        lists:foldl(fun(Item, Acc) -> ast("[$Item | $Acc].", 0) end,
                    ast("[].", 0),
                    lists:reverse(Vars)),
    case Names of
        [] ->
            ast("begin match = re:run($Input, @Mp, [{capture, @Names, list}]) end.", Line);

        [_ | _] ->
            ast("begin {match, $Match}=re:run($Input, @Mp, [{capture, @Names, list}]) end.", Line)
    end.
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


%%--------------------------------------------------------------------
%% STRING PATTERN
%%--------------------------------------------------------------------
parseMatch(ast_pattern("$X."), #?PT_STR_PARSER{exp = Exp} = Parser) when pt_lib:is_string(X) ->
    try
        Value = erl_parse:normalise(X),
        Parser#?PT_STR_PARSER{exp = [Value | Exp]}
    catch
        _:Error ->
            throw(?mk_parse_error(Parser#?PT_STR_PARSER.line, #?PT_STR_PARSER_ERR_BAD_ARG{arg = X, error = Error}))
    end;

%%--------------------------------------------------------------------
%% TYPE PATTERN
%%--------------------------------------------------------------------
parseMatch(ast_pattern("$X."), #?PT_STR_PARSER{exp = Exp, types = Types} = Parser) when pt_lib:is_atom(X) ->
    try
        Value = erl_parse:normalise(X),
        Parser#?PT_STR_PARSER{exp = [getTypeExp(Value, Types) | Exp]}
    catch
        throw:#?PT_STR_PARSER_ERR_UNKNOWN_TYPE{} = Error ->
            throw(?mk_parse_error(Parser#?PT_STR_PARSER.line, Error));

        _:Error ->
            throw(?mk_parse_error(Parser#?PT_STR_PARSER.line, #?PT_STR_PARSER_ERR_BAD_ARG{arg = X, error = Error}))
    end;

%%--------------------------------------------------------------------
%% MATCH
%%--------------------------------------------------------------------
parseMatch(ast_pattern("$X = $Y.", Line) = Z, #?PT_STR_PARSER{exp = Exp,
                                                              bind = Bind,
                                                              count = Count} = Parser) ->
    case pt_lib:is_term_or_var(X) of
        true ->
            I = Count + 1,
            Key = erlang:integer_to_list(I),
            VarKey = list_to_atom(Key),

            NewParser =
                parseMatch(Y, Parser#?PT_STR_PARSER{exp = [], bind = [{X, VarKey} | Bind], count = I}),

            NewParser#?PT_STR_PARSER{exp = [#?PT_STR_PEG_MTC{var = Key, exp = makeExpression(NewParser)} | Exp]};

        _ ->
            throw(?mk_parse_error(Line, #?PT_STR_PARSER_ERR_BAD_MATCH_EXP{exp = Z}))
    end;

%%--------------------------------------------------------------------
%% {[...]} Selector
%%--------------------------------------------------------------------
parseMatch(ast_pattern("{$X}.", Line), #?PT_STR_PARSER{exp = Exp} = Parser) when pt_lib:is_list(X)  ->
    case pt_lib:list_fold(fun(Item, _, Acc) -> parseMatch(Item, Acc) end,
                          Parser#?PT_STR_PARSER{exp = []},
                          X) of

        #?PT_STR_PARSER{exp = []} ->
            throw(?mk_parse_error(Line,
                                  #?PT_STR_PARSER_ERR_BAD_EXP{exp = "{[]}", msg = "Selector should have at least one argument"}));

        #?PT_STR_PARSER{} = NewParser ->
            NewParser#?PT_STR_PARSER{exp = [#?PT_STR_PEG_SEQ{exp = makeSelector(NewParser)} | Exp]}
    end;

%%--------------------------------------------------------------------
%% SEQ
%%--------------------------------------------------------------------
parseMatch(ast_pattern("{ ...$X... }.", Line), #?PT_STR_PARSER{exp = Exp} = Parser) ->
    case X of
        [] ->
            throw(?mk_parse_error(Line,
                                  #?PT_STR_PARSER_ERR_BAD_EXP{exp = "{}", msg = "Sequence expression should have at least one argument"}));

        [_ | _] = Args ->
            NewParser =
                lists:foldl(fun parseMatch/2, Parser#?PT_STR_PARSER{exp = []}, Args),

            NewParser#?PT_STR_PARSER{exp = [#?PT_STR_PEG_SEQ{exp = makeExpression(NewParser)} | Exp]};

         Other ->
            throw(?mk_parse_error(Line,
                                  #?PT_STR_PARSER_ERR_BAD_EXP{exp = Other, msg = "Invalid mandatory expression arguments"}))
    end;

%%--------------------------------------------------------------------
%% OPTIONAL
%%--------------------------------------------------------------------
parseMatch(ast_pattern("$X."), #?PT_STR_PARSER{exp = Exp} = Parser) when pt_lib:is_list(X) ->
    case pt_lib:list_fold(fun(Item, _, Acc) -> parseMatch(Item, Acc) end,
                          Parser#?PT_STR_PARSER{exp = []},
                          X) of

        #?PT_STR_PARSER{exp = []} ->
            throw(?mk_parse_error(Parser#?PT_STR_PARSER.line,
                                  #?PT_STR_PARSER_ERR_BAD_EXP{exp = "[]",
                                                              msg = "Optional expression should have at least one argument"}));

        #?PT_STR_PARSER{} = NewParser ->
            NewParser#?PT_STR_PARSER{exp = [#?PT_STR_PEG_OPT{exp = makeExpression(NewParser)} | Exp]}
    end;

%%--------------------------------------------------------------------
%% Other
%%--------------------------------------------------------------------
parseMatch(Other, #?PT_STR_PARSER{line = Line}) ->
    throw(?mk_parse_error(Line, #?PT_STR_PARSER_ERR_BAD_EXP{exp = Other, msg = "Unknown expression format"})).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


getTypeExp(Type, Types) when is_atom(Type) ->
    case lists:keyfind(Type, 1, Types) of
        {_, Exp} ->
            Exp;

        _ ->
            throw(#?PT_STR_PARSER_ERR_UNKNOWN_TYPE{type = Type})
    end.
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


makeExpression(#?PT_STR_PARSER{exp = []}) ->
    "";

makeExpression(#?PT_STR_PARSER{exp = [_ | _] = Exp}) ->
    string:join(buildExpression(Exp, []), "").
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


makeSelector(#?PT_STR_PARSER{exp = []}) ->
    "";

makeSelector(#?PT_STR_PARSER{exp = [_ | _] = Exp}) ->
    string:join(buildExpression(Exp, []), "|").
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

buildExpression([], Result) ->
    Result;

%%--------------------------------------------------------------------
%% SEQ
%%--------------------------------------------------------------------
buildExpression([#?PT_STR_PEG_SEQ{exp = Exp} | Other ], Result) ->
    buildExpression(Other, ["("++ Exp ++ ")" | Result]);

%%--------------------------------------------------------------------
%% OPT
%%--------------------------------------------------------------------
buildExpression([#?PT_STR_PEG_OPT{exp = Exp} | Other], Result) ->
    buildExpression(Other, ["(" ++ Exp ++ ")?" | Result]);

%%--------------------------------------------------------------------
%% MATCH
%%--------------------------------------------------------------------
buildExpression([#?PT_STR_PEG_MTC{var = Var, exp = Exp} | Other], Result) ->
    buildExpression(Other, ["(?<" ++ Var ++ ">" ++ Exp ++ ")" | Result]);

%%--------------------------------------------------------------------
%% PAT
%%--------------------------------------------------------------------
buildExpression([Exp | Other], Result) when is_list(Exp) ->
    buildExpression(Other, [Exp | Result]).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


buildRegex(#?PT_STR_PARSER{types = Types} = Parser) ->
    Exp = getTypeExp(?PT_STR_PARSER_HEAD, Types) ++ makeExpression(Parser),

    pt_patrol:info("Builded expression:~n~s~n", [Exp]),

    case re:compile(Exp) of
        {ok, _} ->
            Exp;

        {error, Error} ->
            throw(?mk_parse_error(Parser#?PT_STR_PARSER.line,
                                  #?PT_STR_PARSER_ERR_INVAL_EXP{exp = Exp, error = Error}))
    end.
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++