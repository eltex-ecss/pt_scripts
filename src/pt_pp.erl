%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pt_pp).

-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_lib/include/pt_patrol.hrl").
-include("pt_macro.hrl").

-export([parse_transform/2]).

-patrol([{tty, error}]).

parse_transform(AST, _Options) ->
    Params = pt_lib:get_attribute_value(pp, AST),
    pp(AST, Params).

pp(AST, []) -> AST;
pp(AST, undefined) -> AST;
pp(AST, Params) ->
    try
        RecrfClauses = lists:foldl(
            fun ({RName, Subrecords}, Acc) when is_list(Subrecords), is_atom(RName) ->
                    Clauses =   lists:map(
                                    fun (El) when is_atom(El) ->
                                        ast("(@El, RL) ->
                                                RI = record_info(fields, @El),
                                                case length(RI) of
                                                    RL -> RI;
                                                    _ -> no
                                                end.", 0)
                                    end,
                                    [RName | Subrecords]
                                ),

                    Clauses1 = Clauses ++ [ast("(_,_) -> no.", 0)],

                    [ ast("(@RName) -> fun [...$Clauses1...] end.", 0) | Acc];

                ({RName}, Acc) when is_atom(RName) ->
                    [ast("(@RName) -> fun (@RName, RL) ->
                                            RI = record_info(fields, @RName),
                                            case length(RI) of
                                                RL -> RI;
                                                _ -> no
                                            end;
                                         (_,_) -> no
                                     end.",0) | Acc];

                (RName, Acc) -> ?PATROL_ERROR("Bad format of pp parser param: ~p", [RName]), Acc
            end,
            [],
            Params),
        FunAST = ast("rec_rf[...$RecrfClauses...].", 0),
        pt_lib:add_function(AST, FunAST)
    catch
        C:E -> ?PATROL_EXCEPTION("Exception: ~p:~p", [C, E]), AST
    end.