%%% File    : csv2es_compiler.erl

-module(csv2es_compiler).

%%% Exports
-export([compile/1, compile/2, compile/3,
	 compile_file/1, compile_file/2, compile_file/3]).

%%% Defines
-define(DEFAULT_SCANNER, csv2es_s_scan:scanner()).

-define(DEFAULT_PARSER, csv2es_parser:parser()).

-define(DEFAULT_ES_OPTIONS,[{<<"index">>,[{<<"_index">>,<<"csv2es">>},{<<"_type">>,<<"_doc">>}]}]).

%%% API
compile_file(File) -> compile_file(File, []).

compile_file(File, Options) ->
    Scanner = (?DEFAULT_SCANNER),
    compile_file(Scanner, File, Options).

compile_file(Scanner, File, Options) ->
    {ok, Binary} = file:read_file(File),
    compile(Scanner, binary_to_list(Binary), Options).

compile(String) -> compile(String, []).

compile(String, Options) ->
    Scanner = (?DEFAULT_SCANNER),
    compile(Scanner, String, Options).

compile(Scanner, String, Options) ->
    Parser = (?DEFAULT_PARSER),
    {ok, R} = Parser(scan(Scanner, String)),
    P = to_prop_lists(R),
    Mapped = case proplists:get_value(mapping,Options) of
                undefined -> P;
                Mapping -> mapping(Mapping,P)
            end,
    %% jsone wants binaries.... and other stuff
    Adpated = adapt_to_jsone(Mapped),
    %% Create ElasticSearch Bulk Index Format
    TheESoptions = case proplists:get_value(esoptions,Options) of
                    undefined -> ?DEFAULT_ES_OPTIONS;
                    ESOptions -> to_es_options(ESOptions)
                end,
    {TheESoptions,Adpated}.


%%%-------------------------------------------------------------------
%%% Misc functions
%%%-------------------------------------------------------------------

%%% Scan
scan(Scanner, String) ->
    {ok, Tokens, _} = Scanner(String), Tokens.


%%% Create prop list
to_prop_lists([Keys|T]) ->
    to_prop_lists(Keys,T,[]).

to_prop_lists(_,[],PL) ->
    PL;
to_prop_lists(Keys,[H|T],PL) ->
    to_prop_lists(Keys,T,[lists:zip(Keys,H)|PL]).


%% Map
mapping(Mapping,L) ->
    F = fun(KV) -> maybe_do_mapping(Mapping,KV) end,
    lists:map(fun(PL) -> lists:map(F,PL) end, L).

maybe_do_mapping(Mapping, {Key,Value}) ->
    case proplists:get_value(Key,Mapping) of
        undefined -> {Key,Value};
        Type -> {Key,to_type(Type,Value)}
    end.


%%% To type
to_type(integer,V) ->
    list_to_integer(V);
to_type(float,V) ->
    list_to_float(V).


%% Convert K and Vs to binaries
adapt_to_jsone(L) ->
    F = fun(KV) -> do_adapt_to_jsone(KV) end,
    lists:map(fun(PL) -> lists:map(F,PL) end, L).

do_adapt_to_jsone({K,"null"}) ->
    {list_to_binary(K),null};
do_adapt_to_jsone({K,V}) when is_number(V) ->
    {list_to_binary(K),V};
do_adapt_to_jsone({K,V}) ->
    {list_to_binary(K),list_to_binary(V)}.


%% to es options
to_es_options(Options) ->
    Index = proplists:get_value("index",Options),
    Type = proplists:get_value("type",Options),
    [{<<"index">>,[{<<"_index">>,list_to_binary(Index)},{<<"_type">>,list_to_binary(Type)}]}].

