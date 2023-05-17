-module(csv2es).

%% API exports
-export([main/1]).

-define(BOM,65279). %% ZERO WIDTH NO-BREAK SPACE (BYTE ORDER MARK)


%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([]) ->
    help(option_spec());
main(Args) ->
    OptSpec = option_spec(),
    case getopt:parse(OptSpec, Args) of
        {ok, {Options, NonOptArgs}} ->
            case lists:member(help,Options) of
                true ->
                    help(OptSpec);
                _ ->
                    io:format("Options:~n  ~p~n~nNon-option arguments:~n  ~p~n", [Options, NonOptArgs]),
                    AllOptions = esoptions2pl(Options),
                    CSVFileName = proplists:get_value(csv,Options),
                    CSVList = csv2list(CSVFileName),
                    try csv2es_compiler:compile(CSVList)  of
                        PropList ->
                            Jsone = adapt_to_jsone(PropList),
                            ESOptions = to_es_options(proplists:get_value(es_options,AllOptions)),
                            OutputFile = proplists:get_value(file,Options),
                            write_file(ESOptions,Jsone,OutputFile)
                    catch
                        {error, {scanner, Error}} ->
                            print_scanner_error(Error)
                    end
            end;
        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            getopt:usage(OptSpec, escript:script_name())
    end,
    erlang:halt(0).


%%====================================================================
%% Internal functions
%%====================================================================
help(OptSpec) ->
    getopt:usage(OptSpec,escript:script_name()).


option_spec() ->   
[
    %% {Name,     ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
    {help,        $h,        "help",        undefined,             "Show the program options"},
    {es_index,    $i,        "es_index",    {string,"csv2es"},     "ElasticSearch index name"},
    {es_type,     $t,        "es_type",     {string,"_doc"},       "ElasticSearch type name"},
    {csv,         $c,        "csv",         string,                "CSV file"},
    {file,        undefined, undefined,     string,                "Output file"}
].


esoptions2pl(Options) ->
    Index = proplists:get_value(es_index,Options),
    Type = proplists:get_value(es_type,Options),
    [{es_options,[{index,Index},{type,Type}]}].


csv2list(CSV) ->
    {ok, Binary} = file:read_file(CSV),
    List = unicode:characters_to_list(Binary),
    %% Remove BOM
    lists:filter(fun(X) -> X /= ?BOM end,List).


%% Adapt to jsone format
adapt_to_jsone(L) ->
    F = fun(KV) -> do_adapt_to_jsone(KV) end,
    lists:map(fun(PL) -> lists:map(F,PL) end, L).

do_adapt_to_jsone({K,"null"}) ->
    {to_binary(K),null};
do_adapt_to_jsone({K,V}) when is_number(V) ->
    {to_binary(K),V};
do_adapt_to_jsone({K,V}) ->
    {to_binary(K),to_binary(V)}.


%% to binary
to_binary(List) ->
    unicode:characters_to_binary(List).


%% ES options to jsone format
to_es_options(Options) ->
    Index = proplists:get_value(index,Options),
    Type = proplists:get_value(type,Options),
    [{<<"index">>,[{<<"_index">>,list_to_binary(Index)},{<<"_type">>,list_to_binary(Type)}]}].


print_scanner_error({Line,_Module,{illegal,Token}}) ->
    io:format("Illegal token: ~p, ~w, on line: ~p~n",[Token, Token, Line]).


%% This is probably slow
write_file(ESOPtions,Proplist,File) ->
    lists:foreach(fun(L) -> file:write_file(File,[jsone:encode(ESOPtions),<<"\n">>,jsone:encode(L),<<"\n">>],[append]) end,Proplist).
