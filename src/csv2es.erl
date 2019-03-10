-module(csv2es).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([]) ->
    getopt:usage(option_spec(), escript:script_name());
main(Args) ->
    OptSpec = option_spec(),
    case getopt:parse(OptSpec, Args) of
        {ok, {Options, NonOptArgs}} ->
            case lists:member(help,Options) of
                true ->
                    getopt:usage(OptSpec,escript:script_name());
                _ ->
                    io:format("Options:~n  ~p~n~nNon-option arguments:~n  ~p~n", [Options, NonOptArgs]),
                    AllOptions = esoptions2pl(Options),
                    CSVFileName = proplists:get_value(csv,Options),
                    CSVString = csv2string(CSVFileName),
                    {ESOPtions,Proplist} = csv2es_compiler:compile(CSVString,AllOptions),
                    OutputFile = proplists:get_value(file,Options),
                    write_file(ESOPtions,Proplist,OutputFile)
            end;
        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            getopt:usage(OptSpec, escript:script_name())
    end,
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
esoptions2pl(Options) ->
    Index = proplists:get_value(es_index,Options),
    Type = proplists:get_value(es_type,Options),
    [{es_options,[{index,Index},{type,Type}]}].


csv2string(CSV) ->
    {ok, Binary} = file:read_file(CSV),
    binary_to_list(Binary).


write_file(ESOPtions,Proplist,File) ->
    lists:foreach(fun(L) -> file:write_file(File,[jsone:encode(ESOPtions),<<"\n">>,jsone:encode(L),<<"\n">>],[append]) end,Proplist).


option_spec() ->   
[
    %% {Name,     ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
    {help,        $h,        "help",        undefined,             "Show the program options"},
    {es_index,    $i,        "es_index",    {string,"csv2es"},     "ElasticSearch index name"},
    {es_type,     $t,        "es_type",     {string,"_doc"},       "ElasticSearch type name"},
    {csv,         $c,        "csv",         string,                "CSV file"},
    {file,        undefined, undefined,     string,                "Output file"}
].
