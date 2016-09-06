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
                RecFields = parse_record(RecType, RecInfo, Exclude),
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

parse_record(RecType, RecInfo, Exclude) ->
    lists:reverse(parse_record(RecType, RecInfo, [], Exclude)).

parse_record(RecType, [{record_field, _, {atom, _, Name}} | Tile],
             Acc, Exclude) ->
    Type = get_type(Name, RecType, Exclude),
    parse_record(RecType, Tile, [{Name, Type} | Acc], Exclude);
parse_record(RecType, [{record_field, _, {atom, _, Name}, _} | Tile],
             Acc, Exclude) ->
    Type = get_type(Name, RecType, Exclude),
    parse_record(RecType, Tile, [{Name, Type} | Acc], Exclude);
parse_record(RecType, [_ | Tile], Acc, Exclude) ->
    parse_record(RecType, Tile, Acc, Exclude);
parse_record(_, [], Acc, _Exclude) -> Acc.

get_type(Name, [{record_field, _, {atom, _, Name}, _} | _], _) ->
    undefined;
get_type(Name, [{record_field, _, {atom, _, Name}} | _], _) ->
    undefined;
get_type(Name, [{typed_record_field, {record_field, _, {atom, _, Name}, _},
                 TypeInfo} | _], Exclude) ->
    get_type(TypeInfo, Exclude);
get_type(Name, [{typed_record_field, {record_field, _, {atom, _, Name}},
                 TypeInfo} | _], Exclude) ->
    get_type(TypeInfo, Exclude);
get_type(Name, [_ | Tail], Exclude) ->
    get_type(Name, Tail, Exclude);
get_type(_, [], _) -> undefined.

get_type({type, _, record, [{atom, _, Name}]}, Exclude) ->
    case lists:member(Name, Exclude) of
        false ->
            {record, Name};
        true ->
            undefined
    end;
get_type({type, _, Type, []}, _Exclude) ->
    Type;
get_type({type, _, list, Info}, Exclude) ->
    lists:map(
      fun(TypeInf) ->
              get_type(TypeInf, Exclude)
      end, Info);
get_type({type, _, union, Info}, Exclude) ->
    Res = lists:filter(fun filter_undefined/1,
                lists:map(
            fun(TypeInf) ->
                    get_type(TypeInf, Exclude)
            end, Info)),
    case lists:usort(Res) of
        [SingleType] -> SingleType;
        [] -> undefined;
        Other -> {union, Other}
    end;
get_type(_, _) ->
    undefined.

filter_undefined(undefined) -> false;
filter_undefined(_) -> true.
