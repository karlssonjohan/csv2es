%% File:        csv2es_parser.yrl

Nonterminals Row elements.

Terminals string newline.

Rootsymbol Row.


% Start
Row -> elements: ['$1'].
Row -> elements newline: ['$1'].
Row -> elements newline Row: ['$1'] ++ '$3'.

elements -> string: [v('$1')].
elements -> string elements: [v('$1')] ++ '$2'.


Erlang code.

-export([parser/0]).

parser() ->
  fun(Tokens) -> ?MODULE:parse(Tokens) end.

v(Token) ->
  element(3,Token).