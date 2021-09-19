-module(emojifyme).
-export([append/1, reverse/1, nth/2, prefix/2, last/1, sum/1, duplicate/2,
         min/1, max/2, delete/2, zip/2, flatten/2, enumerate/2, all/2, any/2,
         map/2, flatmap/2, filter/2, foldl/3, foreach/2, search/2, split/2,
         join/2, mean/1, binary_search/4, sort/3, qsort/1, fibonacci/1,
         factorial/1, hanoi/4]).

append([E]) ->
    E;
append([H | T]) ->
    H ++ append(T);
append([]) ->
    [].

reverse([] = L) ->
    L;
reverse([_] = L) ->
    L;
reverse([A, B]) ->
    [B, A];
reverse([A, B | L]) ->
    lists:reverse(L, [B, A]).

nth(1, [H | _]) ->
    H;
nth(N, [_ | T]) when N > 1 ->
    nth(N - 1, T).

prefix([X | PreTail], [X | Tail]) ->
    prefix(PreTail, Tail);
prefix([], List) when is_list(List) ->
    true;
prefix([_ | _], List) when is_list(List) ->
    false.

last([E | Es]) ->
    last(E, Es).

last(_, [E | Es]) ->
    last(E, Es);
last(E, []) ->
    E.

sum(L) ->
    sum(L, 0).

sum([H | T], Sum) ->
    sum(T, Sum + H);
sum([], Sum) ->
    Sum.

duplicate(N, X) when is_integer(N), N >= 0 ->
    duplicate(N, X, []).

duplicate(0, _, L) ->
    L;
duplicate(N, X, L) ->
    duplicate(N - 1, X, [X | L]).

min([H | T]) ->
    min(T, H).

min([H | T], Min) when H < Min ->
    min(T, H);
min([_ | T], Min) ->
    min(T, Min);
min([], Min) ->
    Min.

max([H | T]) ->
    max(T, H).

max([H | T], Max) when H > Max ->
    max(T, H);
max([_ | T], Max) ->
    max(T, Max);
max([], Max) ->
    Max.

delete(Item, [Item | Rest]) ->
    Rest;
delete(Item, [H | Rest]) ->
    [H | delete(Item, Rest)];
delete(_, []) ->
    [].

zip([X | Xs], [Y | Ys]) ->
    [{X, Y} | zip(Xs, Ys)];
zip([], []) ->
    [].

flatten(List, Tail) when is_list(List), is_list(Tail) ->
    do_flatten(List, Tail).

do_flatten([H | T], Tail) when is_list(H) ->
    do_flatten(H, do_flatten(T, Tail));
do_flatten([H | T], Tail) ->
    [H | do_flatten(T, Tail)];
do_flatten([], Tail) ->
    Tail.

enumerate(Index, [H | T]) when is_integer(Index) ->
    [{Index, H} | enumerate(Index + 1, T)];
enumerate(Index, []) when is_integer(Index) ->
    [].

all(Pred, [Hd | Tail]) ->
    case Pred(Hd) of
        true ->
            all(Pred, Tail);
        false ->
            false
    end;
all(Pred, []) when is_function(Pred, 1) ->
    true.

any(Pred, [Hd | Tail]) ->
    case Pred(Hd) of
        true ->
            true;
        false ->
            any(Pred, Tail)
    end;
any(Pred, []) when is_function(Pred, 1) ->
    false.

map(F, [H | T]) ->
    [F(H) | map(F, T)];
map(F, []) when is_function(F, 1) ->
    [].

flatmap(F, [Hd | Tail]) ->
    F(Hd) ++ flatmap(F, Tail);
flatmap(F, []) when is_function(F, 1) ->
    [].

filter(Pred, List) when is_function(Pred, 1) ->
    [E || E <- List, Pred(E)].

foreach(F, [Hd | Tail]) ->
    F(Hd),
    foreach(F, Tail);
foreach(F, []) when is_function(F, 1) ->
    ok.

foldl(F, Accu, [Hd | Tail]) ->
    foldl(F, F(Hd, Accu), Tail);
foldl(F, Accu, []) when is_function(F, 2) ->
    Accu.

search(Pred, [Hd | Tail]) ->
    case Pred(Hd) of
        true ->
            {value, Hd};
        false ->
            search(Pred, Tail)
    end;
search(Pred, []) when is_function(Pred, 1) ->
    false.

split(N, List) when is_integer(N), N >= 0, is_list(List) ->
    case split(N, List, []) of
        {_, _} = Result ->
            Result;
        Fault when is_atom(Fault) ->
            erlang:error(Fault, [N, List])
    end;
split(N, List) ->
    erlang:error(badarg, [N, List]).

split(0, L, R) ->
    {lists:reverse(R, []), L};
split(N, [H | T], R) ->
    split(N - 1, T, [H | R]);
split(_, [], _) ->
    badarg.

join(_Sep, []) ->
    [];
join(Sep, [H | T]) ->
    [H | join_prepend(Sep, T)].

join_prepend(_Sep, []) ->
    [];
join_prepend(Sep, [H | T]) ->
    [Sep, H | join_prepend(Sep, T)].

mean([]) ->
    0;
mean(L) ->
    lists:sum(L) / erlang:length(L).

binary_search(List, Value, Low, High) ->
    if Low > High ->
           not_found;
       true ->
           Mid = (Low + High) div 2,
           MidNum = lists:nth(Mid, List),
           if MidNum > Value ->
                  binary_search(List, Value, Low, Mid - 1);
              MidNum < Value ->
                  binary_search(List, Value, Mid + 1, High);
              true ->
                  Mid
           end
    end.

factorial(N) ->
    factorial(N - 1, N).

factorial(1, N) ->
    N;
factorial(I, N) ->
    factorial(I - 1, N * I).

fibonacci(0) ->
    0;
fibonacci(1) ->
    1;
fibonacci(N) ->
    fibonacci(N - 1) + fibonacci(N - 2).

sort([], Acc, true) ->
    lists:reverse(Acc);
sort([], Acc, false) ->
    sort(lists:reverse(Acc), [], true);
sort([X, Y | T], Acc, _Done) when X > Y ->
    sort([X | T], [Y | Acc], false);
sort([X | T], Acc, Done) ->
    sort(T, [X | Acc], Done).

qsort([]) ->
    [];
qsort([X | Xs]) ->
    qsort([Y || Y <- Xs, Y < X]) ++ [X] ++ qsort([Y || Y <- Xs, Y >= X]).

hanoi(1, F, T, _V) ->
    io:format("from ~p to ~p~n", [F, T]);
hanoi(N, F, T, V) ->
    hanoi(N - 1, F, V, T),
    hanoi(1, F, T, V),
    hanoi(N - 1, V, T, F).
