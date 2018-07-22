%% File:        csv2es_parser.yrl

Nonterminals Row elements element.

Terminals string newline.

Rootsymbol Row.


% Start
Row -> elements newline: ['$1'].
Row -> elements newline Row: ['$1'] ++ '$3'.

elements -> element: ['$1'].
elements -> element elements: ['$1'] ++ '$2'.

element -> string : v('$1').


Erlang code.

-export([parser/0]).

parser() ->
  fun(Tokens) -> ?MODULE:parse(Tokens) end.

v(Token) ->
  element(3,Token).
