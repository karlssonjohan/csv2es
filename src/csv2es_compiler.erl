%%% File    : csv2es_compiler.erl

-module(csv2es_compiler).

%%% Exports
-export([compile/1, compile/2, compile/3]).

%%% Defines
-define(DEFAULT_SCANNER, csv2es_s_scan:scanner()).

-define(DEFAULT_PARSER, csv2es_parser:parser()).

%-define(DEFAULT_ES_OPTIONS,[{<<"index">>,[{<<"_index">>,<<"csv2es">>},{<<"_type">>,<<"_doc">>}]}]).

%%% API
compile(String) -> compile(String, []).

compile(String, Options) ->
    Scanner = (?DEFAULT_SCANNER),
    compile(Scanner, String, Options).

compile(Scanner, String, Options) ->
    Parser = (?DEFAULT_PARSER),
    {ok, R} = Parser(scan(Scanner, String)),
    P = to_prop_lists(R),
    AdpatedList = adapt_to_jsone(P),
    TheESoptions = to_es_options(proplists:get_value(es_options,Options)),
    {TheESoptions,AdpatedList}.


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


%% Adapt to jsone format
adapt_to_jsone(L) ->
    F = fun(KV) -> do_adapt_to_jsone(KV) end,
    lists:map(fun(PL) -> lists:map(F,PL) end, L).

do_adapt_to_jsone({K,"null"}) ->
    {list_to_binary(K),null};
do_adapt_to_jsone({K,V}) when is_number(V) ->
    {list_to_binary(K),V};
do_adapt_to_jsone({K,V}) ->
    {list_to_binary(K),list_to_binary(V)}.


%% ES options to jsone format
to_es_options(Options) ->
    Index = proplists:get_value(index,Options),
    Type = proplists:get_value(type,Options),
    [{<<"index">>,[{<<"_index">>,list_to_binary(Index)},{<<"_type">>,list_to_binary(Type)}]}].

