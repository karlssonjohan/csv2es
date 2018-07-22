-module(csv2es).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([]) ->
    getopt:usage(option_spec_list(), escript:script_name());
main(Args) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, NonOptArgs}} ->
            io:format("Options:~n  ~p~n~nNon-option arguments:~n  ~p~n", [Options, NonOptArgs]),
            A = esoptions2pl(Options),
            B = typespecoptions2pl(Options),
            io:format("~p~n",[B]),
            AllOptions = lists:append(A,B),
            CSVFileName = proplists:get_value(csv,Options),
            CSVString = csv2string(CSVFileName),
            {ESOPtions,Proplist} = csv2es_compiler:compile(CSVString,AllOptions),
            OutputFile = proplists:get_value(file,Options),
            write_file(ESOPtions,Proplist,OutputFile);
        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            getopt:usage(OptSpecList, escript:script_name())
    end,
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
esoptions2pl(Options) ->
    Index = proplists:get_value(es_index,Options),
    Type = proplists:get_value(es_type,Options),
    [{es_options,[{index,Index},{type,Type}]}].

typespecoptions2pl(Options) ->
    L = proplists:get_all_values(type_spec,Options),
    NewL = lists:map(fun(S) -> lists:map(fun(SS) -> string:trim(SS) end, string:split(S,":")) end,L),
    [{type_spec,[{K,list_to_atom(V)}|| [K,V] <- NewL]}].



csv2string(CSV) ->
    {ok, Binary} = file:read_file(CSV),
    binary_to_list(Binary).

write_file(ESOPtions,Proplist,File) ->
    lists:foreach(fun(L) -> file:write_file(File,[jsone:encode(ESOPtions),<<"\n">>,jsone:encode(L),<<"\n">>],[append]) end,Proplist).



option_spec_list() ->   
[
    %% {Name,     ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
    {help,        $?,        "help",        undefined,             "Show the program options"},
    {es_index,    $i,        "es_index",    {string,"csv2es"},     "ElasticSearch index name"},
    {es_type,     $t,        "es_type",     {string,"_doc"},       "ElasticSearch type name"},
    {csv,         $c,        "csv",         string,                "CSV file"},
    {type_spec,   $s,        "type_spec",   string,                "Type specification"},
    {file,        undefined, undefined,     string,                "Output file"}
].
