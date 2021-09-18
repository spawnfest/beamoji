-module(pt_test_1).

-export([main/1]).

main(foo) ->
    foo = foo,
    io:format("hello, ~p!~n", [world]),
    {bar, [baz]}.
