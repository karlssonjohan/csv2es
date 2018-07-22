%% File:         csv2es_s_scan.xrl
%% Description:  Scans CSV files with ";" as field delimiter. 
%%               "%%" is the comment token.

Definitions.

T = [0-9a-zA-Zåäö\.\-\,\s@]


Rules.

{T}+ : 
              {token,{string,TokenLine,TokenChars}}.
\;\; :        {token,{string,TokenLine,"null"}}.
\r\n :        {token,{newline,TokenLine}}.
\n :          {token,{newline,TokenLine}}.
\%\%.*\r\n :  skip_token.
\; :          skip_token.


Erlang code.

-export([scanner/0]).

scanner() ->
  fun(String) -> ?MODULE:string(String) end.
