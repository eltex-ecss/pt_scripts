%%%----------------------------------------------------------------------------
%%% @author platinumthinker <platinumthinker@gmail.com>
%%% @doc
%%% Add several function in module:
%%%   all_records returned list all name records defined for this module.
%%%   properties returned list strings property composed from record name and
%%%   property name.
%%%
%%% Supports custom attributes:
%%% -include_records([record_a, record_b]).
%%% -exclude_records([record_c]).
%%%
%%% List records = (All records or Include records (if defined)) - Exclude records.
%%% @end
%%%----------------------------------------------------------------------------

-module(pt_record_properties).

-include_lib("pt_lib/include/pt_error_macro.hrl").
-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_scripts/include/pt_recompilable.hrl").

-export([
    parse_transform/2,
    format_error/1
]).

-spec parse_transform(AST :: list(), Options :: list()) -> list().
parse_transform(AST, _Options) ->
    Include = pt_lib:get_attribute_value(include_records, AST),
    Exclude = pt_lib:get_attribute_value(exclude_records, AST),
    Records = [ Rec || Rec = {R, _} <- pt_lib:get_attribute_value(record, AST),
                     (Include == [] orelse lists:member(R, Include))
                     andalso not lists:member(R, Exclude)],
    Types = [ {Name, Type} || {{record, Name}, Type, _} <-
                pt_lib:get_attribute_value(type, AST) ],
    RecordsName = lists:sort(lists:foldl(
        fun({RecName, _}, Acc) -> [RecName | Acc];
           (_, Acc) -> Acc
        end, [], Records)),
    AST1 = pt_lib:add_function(AST, ast("all_records() -> @RecordsName.", 0)),
    Function = lists:foldl(
        fun({RecName, RecInfo}, AccFunctions) ->
                RecType = proplists:get_value(RecName, Types, []),
                RecFields = parse_record(RecType, RecInfo),
                Clause = io_lib:fwrite("properties(~p) -> ~w", [RecName, RecFields]),
                [ lists:flatten(Clause) | AccFunctions];
           (_, Acc) -> Acc
        end, [], Records),
    [ASTFuncProperties] = pt_lib:str2ast(string:join(Function, ";") ++
        ";properties(_) -> {error, no_found}.", 0),
    pt_lib:add_function(AST1, ASTFuncProperties).

-spec format_error(Error :: term()) -> string().
format_error(Error) ->
    io:format("Error: ~p~n", [Error]).

parse_record(RecType, RecInfo) ->
    parse_record(RecType, RecInfo, []).

parse_record(RecType, [{record_field, _, {atom, _, Name}} | Tile], Acc) ->
    Type = get_type(Name, RecType),
    parse_record(RecType, Tile, [{Name, Type} | Acc]);
parse_record(RecType, [{record_field, _, {atom, _, Name}, _} | Tile], Acc) ->
    Type = get_type(Name, RecType),
    parse_record(RecType, Tile, [{Name, Type} | Acc]);
parse_record(RecType, [_ | Tile], Acc) ->
    parse_record(RecType, Tile, Acc);
parse_record(_, [], Acc) -> Acc.

get_type(Name, [{typed_record_field, {record_field, _, {atom, _, Name}, _},
                 {type, _, Type = list, [{type, _, record, _}]}} | _]) ->
    {Type, record};
get_type(Name, [{typed_record_field, {record_field, _, {atom, _, Name}, _},
                 {type, _, Type, _}} | _]) ->
    Type;
get_type(Name, [_ | Tail]) -> get_type(Name, Tail);
get_type(_, []) -> undefined.
