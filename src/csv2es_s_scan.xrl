%% -*- coding: utf-8 -*-
%% File:         csv2es_s_scan.xrl
%% Description:  Scans CSV files with ";" as field delimiter. 
%%               "#" is the comment token.

Definitions.

T = [0-9a-zA-ZåäöÅÄÖ\.\-\,\s@_µβ)(:%&è<>é/üêàô'+â]
SC = ;
NL = (\r\n|\n|\r)
COMMENT = #.*


Rules.

{T}+ :          {token,{string,TokenLine,TokenChars}}.
{SC} :          skip_token.
{NL} :          {token,{newline,TokenLine}}.
%% Pushback ruins line numbering... :(
{SC}{SC} :      {token,{string,TokenLine,"null"},";"}.
{SC}{NL} :      {token,{string, TokenLine,"null"},"\r\n"}.
{COMMENT}{NL} : skip_token.


Erlang code.

-export([scanner/0]).

scanner() ->
  fun(String) -> ?MODULE:string(String) end.
