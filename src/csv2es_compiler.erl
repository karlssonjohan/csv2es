%%% File    : csv2es_compiler.erl

-module(csv2es_compiler).

%%% Exports
-export([compile/1, compile/2]).

%%% Defines
-define(DEFAULT_SCANNER, csv2es_s_scan:scanner()).
-define(DEFAULT_PARSER, csv2es_parser:parser()).


%%% API
compile(String) ->
    Scanner = (?DEFAULT_SCANNER),
    compile(Scanner, String).

compile(Scanner, String) ->
    Parser = (?DEFAULT_PARSER),
    Tokens = scan(Scanner,String),
    {ok, R} = Parser(Tokens),
    to_prop_lists(R).


%%%-------------------------------------------------------------------
%%% Misc functions
%%%-------------------------------------------------------------------

%%% Scan
scan(Scanner, String) ->
    case Scanner(String) of
        {ok, Tokens, _} -> Tokens;
        {error, Error, _ } -> throw({error, {scanner, Error}})
    end.


%%% Create prop list
to_prop_lists([Keys|T]) ->
    to_prop_lists(Keys,T,[]).

to_prop_lists(_,[],PL) ->
    PL;
to_prop_lists(Keys,[Values|T],PL) ->
    to_prop_lists(Keys,T,[lists:zip(Keys,Values)|PL]).
