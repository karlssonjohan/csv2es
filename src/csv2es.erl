-module(csv2es).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    Options = [{mapping,[{"trades",integer},{"low price",float}]},{esoptions,[{"index","myindex"},{"type","_doc"}]}],
    {ESOPtions,Proplist} = csv2es_compiler:compile_file("omxs30.csv",Options),
    write_file(ESOPtions,Proplist),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
write_file(ESOPtions,Proplist) ->
    lists:foreach(fun(L) -> file:write_file("filen",[jsone:encode(ESOPtions),<<"\n">>,jsone:encode(L),<<"\n">>],[append]) end,Proplist).
    %%lists:foreach(fun(L) -> file:write_file("filen",[<<"hej\n">>],[append]) end,Proplist).