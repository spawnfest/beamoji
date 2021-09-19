-module(function_composition).

-define(AND_MACROS, and_macros).
-define(MACROS, macros).

-export([local_calls/3, external_calls/0]).

-type t() :: only:function(application:is(a:ffected(by:this(change)))) | not_types.

local_calls(should, be, unaffected) ->
    g(f(b, h(a), w(x(y)))).

g(X) -> X.
f(X,Y,Z) -> {X,Y,Z}.
h(X) -> X.
w(X) -> X.
x(X) -> X.
macros(X) -> X.
this_one(X) -> X.
more(X) -> X.
is(X,Y,Z) -> {X,Y,Z}.

external_calls() ->
    should:be(
        indented:every(
            singe:time([{even, "when", including}]), local_calls(?AND_MACROS, #{}, undefined)),
        local_calls(?MACROS(should), be, unaffected)),
    the_idea_is_to_force:long_module_and_function_names(
        to_be_put:in_the_next_row([{so, that},
                                   "their",
                                   <<"parameters">>,
                                   can:be(),
                                   read,
                                   more,
                                   easily])),
    this_one(will:be(more(complex:since(
                              it:combines(), local:and_remote(calls))),
                     hopefully:it(is(a, rare, thing)))).
