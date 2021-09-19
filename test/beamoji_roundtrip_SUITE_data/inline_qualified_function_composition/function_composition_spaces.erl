-module(function_composition_spaces).

-define(AND_MACROS, and_macros).
-define(MACROS, macros).

-format #{inline_qualified_function_composition => true, spaces_around_arguments => true}.

-export([local_calls/3, external_calls/0]).

-type t() :: only:function(application:is(a:ffected(by:this(change)))) | not_types.

local_calls(should, be, unaffected) ->
    g( f( b, h( a ), w( x( y ) ) ) ).

g(X) -> X.
f(X,Y,Z) -> {X,Y,Z}.
h(X) -> X.
w(X) -> X.
x(X) -> X.
macros(X) -> X.
within(X) -> X.
the(X) -> X.
parenthesis() -> x.

external_calls() ->
    shouldnt:be(
        indented:every(
            singe:time( [{even, "when", including}] ), local_calls( ?AND_MACROS, #{}, undefined )
        ),
        local_calls( ?MACROS(should), be, unaffected )
    ),
    but:the(
        whole:code( [{should, be}, "nicely", <<"indented">>, now:that(), we, have, spaces] )
    ),
    within( the( parenthesis() ) ).
