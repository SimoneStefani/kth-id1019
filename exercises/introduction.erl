%%%-------------------------------------------------------------------
%%% @author Simone Stefani
%%% @copyright (C) 2017, simonestefani.me
%%% Created : 17. Jan 2017 18:36
%%%-------------------------------------------------------------------
-module(introduction).

-compile(export_all).


%%%-------------------------------------------------------------------
%%% Section 2: A first program
%%%-------------------------------------------------------------------

double(N) ->
  N * 2.

fahrenheitToCelsius(Degrees) ->
  (Degrees - 32) / 1.8.

areaOfRectangle(Height, Width) ->
  Height * Width.

areaOfSquare(Side) ->
  areaOfRectangle(Side, Side).

areaOfCircle(Radius) ->
  2 * math:pi() * Radius.


%%%-------------------------------------------------------------------
%%% Section 3: Recursive definitions
%%%-------------------------------------------------------------------

product(M, N) ->
  if
    M == 0 -> 0;
    true -> N + product(M - 1, N)
  end.

product2(0, _) -> 0;
product2(M, N) -> N + product2(M - 1, N).

product3(M, N) ->
  case M of
    0 -> 0;
    _ -> N + product3(M - 1, N)
  end.

exp(_, 0) -> 1;
exp(X, Y) -> product(X, exp(X, Y - 1)).

exp2(X, 1) -> X;
exp2(X, N) ->
  case (N rem 2) of
    0 -> exp2(X, N div 2) * exp2(X, N div 2);
    _ -> exp2(X, (N - 1)) * X
  end.


%%%-------------------------------------------------------------------
%%% Section 4: List operations
%%%-------------------------------------------------------------------

nth(_, []) -> io:format("The element doesn't exists!~n");
nth(1, [H|_]) -> H;
nth(N, [_|T]) -> nth(N-1, T).

number([]) -> 0;
number([_|T]) -> 1 + number(T).

number2(L) -> number2(L, 0).
number2([], C) -> C;
number2([_|T], C) -> number2(T, C+1).

sum([]) -> 0;
sum([H|T]) -> H + sum(T).

sum2(L) -> sum2(L, 0).
sum2([], S) -> S;
sum2([H|T], S) -> sum2(T, S + H).

duplicate(L) -> duplicate(L, []).
duplicate([], D) -> D;
duplicate([H|T], D) -> duplicate(T, D++[H,H]).

duplicate2([]) -> [];
duplicate2([H|T]) -> [H,H|duplicate2(T)].

member(_, []) -> false;
member(M, [M|_]) -> true;
member(M, [_|T]) -> member(M, T).

unique(L) -> unique(L, []).
unique([], U) -> U;
unique([H|T], U) ->
  case member(H,U) of
    true -> unique(T,U);
    false -> unique(T,U++[H])
  end.

reverse([]) -> [];
reverse([H|T]) -> reverse(T)++[H].

reverse2(L) -> reverse2(L, []).
reverse2([], R) -> R;
reverse2([H|T], R) -> reverse2(T, [H|R]).


%%%-------------------------------------------------------------------
%%% Section 5: Sort
%%%-------------------------------------------------------------------

%%% Insertion Sort
isort(L) -> isort(L, []).
isort([],S) -> S;
isort([H|T], S) -> isort(T, insert(H,S)).

insert(E, []) -> [E];
insert(E, [H|T]=L) ->
  if
    E < H -> [E|L];
    true -> [H|insert(E,T)]
  end.


%%% Merge Sort
msort([H|[]]) ->[H];
msort(L) ->
  {A, B} = msplit(L, [], []),
  merge(msort(A), msort(B)).

msplit([], A, B) -> {A, B};
msplit([H|T], A, B) -> msplit(T, [H|B], A).

merge(A, []) -> A;
merge([], B) -> B;
merge([HA|TA]=A, [HB|TB]=B) ->
  if
    HA < HB -> [HA|merge(TA,B)];
    true -> [HB|merge(A,TB)]
  end.


%%% QuickSort
qsort([]) -> [];
qsort([H|T]) ->
  {SS, LS} = qsplit(T, H, [], []),
  qsort(SS) ++ [H] ++ qsort(LS).

qsplit([], _, A, B) -> {A, B};
qsplit([H|T], P, A, B) ->
  if
    H < P -> qsplit(T, P, [H|A], B);
    true -> qsplit(T, P, A, [H|B])
  end.


quick([]) -> [];
quick([P|L]) ->
  quick([X||X <- L, X < P]) ++ [P] ++ quick([X||X <- L, X >= P]).


fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-2) + fib(N-1).


%%%-------------------------------------------------------------------
%%% Section 6: Trees
%%%-------------------------------------------------------------------

% introduction:treeMember(2, {node, 38, {node, 40, {leaf, 42}, {leaf, 39}}, {leaf, 34}}).
treeMember(_, nil) -> false;
treeMember(N, {leaf, N}) -> true;
treeMember(_, {leaf, _}) -> false;
treeMember(N, {node, N, _, _}) -> true;
treeMember(N, {node, _, L, R}) ->
  case treeMember(N, L) of
    true -> true;
    false -> treeMember(N, R)
  end.

% introduction:lookup(q, {node, k, 38, {node, b, 34, nil, nil}, {node, o, 40, {node, l, 42, nil, nil}, {node, q, 39, nil, nil}}})
lookup(_, nil) -> no;
lookup(Key, {node, Key, V, _, _}) -> V;
lookup(Key, {node, K, _, L, R}) ->
  if
    Key < K -> lookup(Key, L);
    true -> lookup(Key, R)
  end.

% T = introduction:modify(q, 55, {node, k, 38, {node, b, 34, nil, nil}, {node, o, 40, {node, l, 42, nil, nil}, {node, q, 39, nil, nil}}}).
% introduction:lookup(q, T).
modify(_, _, nil) -> nil;
modify(Key, Val, {node, Key, _, L, R}) ->
  {node, Key, Val, L, R};
modify(Key, Val, {node, K, V, L, R}) ->
  if
    Key < K -> {node, K, V, modify(Key, Val, L), R};
    true -> {node, K, V, L, modify(Key, Val, R)}
  end.

% T = introduction:insert(m, 55, {node, k, 38, {node, b, 34, nil, nil}, {node, o, 40, {node, l, 42, nil, nil}, {node, q, 39, nil, nil}}}).
insert(Key, Value, nil) -> {node, Key, Value, nil, nil};
insert(Key, Value, {node, K, V, L, R}) ->
  if
    Key < K -> {node, K, V, insert(Key, Value, L), R};
    true -> {node, K, V, L, insert(Key, Value, R)}
  end.

% TODO Delete

