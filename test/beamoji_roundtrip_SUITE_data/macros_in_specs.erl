-module(macros_in_specs).

-type t() :: t.
-type foo() :: foo.

-define(FOO, foo).

%% @doc Is properly formatted
-spec ?MODULE:f() -> t().
f() ->
    t.

%% @doc Was crashing the formatter
-spec g() -> ?MODULE:t().
g() ->
    t.

%% @doc Can't be parsed
-spec macros_in_specs : ?FOO( ) -> t( ) .

foo() ->
    t.

%% @doc Was crashing the formatter
-spec h() -> macros_in_specs:?FOO().
h() ->
    foo.
