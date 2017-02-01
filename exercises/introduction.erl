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

product(0, _) ->
  0;
product(M, N) ->
  N + product(M - 1, N).

exp(X, 1) ->
  X;
exp(X, Y) ->
  product(X, exp(X, Y - 1)).


fastExp(X, 1) ->
  X;
fastExp(X, Y) ->
  case Y rem 2 of
    1 -> X * fastExp(X * X, (Y - 1) div 2);
    0 -> fastExp(X * X, (Y div 2))
  end.


%%%-------------------------------------------------------------------
%%% Section 4: List operations
%%%-------------------------------------------------------------------

nth(N, L) ->
  case N of
    1 -> [Head | _] = L,
      Head;
    _ -> [_ | Tail] = L,
      nth(N - 1, Tail)
  end.


number(L) ->
  number(L, 0).
number(L, N) ->
  case L of
    [] -> N;
    _ -> [_ | Tail] = L,
      number(Tail, N + 1)
  end.


sum([]) -> 0;
sum([H | T]) -> H + sum(T).


duplicate([]) -> [];
duplicate([H | T]) -> [H, H | duplicate(T)].


unique(L) ->
  unique(L, []).
unique([], Uni) ->
  reverse(Uni);
unique([H | T], Uni) ->
  case lists:member(H, Uni) of
    false -> unique(T, [H | Uni]);
    true -> unique(T, Uni)
  end.


reverse(L) ->
  reverse(L, []).
reverse([], Rev) ->
  Rev;
reverse([H | T], Rev) ->
  reverse(T, [H | Rev]).


pack([]) ->
  [];
pack([H | []]) ->
  [H];
pack([H | T]) ->
  pack(T, [H], []).

pack([], Temp, Acc) ->
  lists:reverse([Temp | Acc]);
pack([H | T], Temp, Acc) when H == hd(Temp) ->
  pack(T, [H | Temp], Acc);
pack([H | T], Temp, Acc) ->
  pack(T, [H], [Temp | Acc]).


% Insertion sort
isort(List) ->
  isort(List, []).

isort([], Sorted) ->
  Sorted;
isort([H | T], Sorted) ->
  isort(T, insert(H, Sorted)).

insert(Element, []) ->
  [Element];
insert(Element, [H | T]) ->
  if
    Element < H -> [Element | [H | T]];
    true -> [H | insert(Element, T)]
  end.


% Merge sort
msort([]) ->
  [];
msort([U]) ->
  [U];
msort(List) ->
  {Left, Right} = msplit(List, [], []),
  merge(msort(Left), msort(Right)).

msplit([], Left, Right) ->
  {Left, Right};
msplit([H | T], Left, Right) ->
  msplit(T, Right, [H | Left]).

merge(L, []) -> L;
merge([], R) -> R;
merge([LH | LT], [RH | RT]) ->
  if
    LH < RH -> [LH | merge(LT, [RH | RT])];
    true -> [RH | merge([LH | LT], RT)]
  end.


% Quicksort
qsort([]) -> [];
qsort([Pivot | T]) ->
  {Small, Large} = qsplit(Pivot, T, [], []),
  SmallSorted = qsort(Small),
  LargeSorted = qsort(Large),
  SmallSorted ++ [Pivot] ++ LargeSorted.

qsplit(_, [], Small, Large) ->
  {Small, Large};
qsplit(Pivot, [H | T], Small, Large) ->
  if
    H =< Pivot -> qsplit(Pivot, T, [H | Small], Large);
    H > Pivot -> qsplit(Pivot, T, Small, [H | Large])
  end.


%%%-------------------------------------------------------------------
%%% Section 5: Reverse
%%%-------------------------------------------------------------------

nreverse([]) -> [];
nreverse([H | T]) ->
  R = nreverse(T),
  lists:append(R, [H]).

% For accumulator reverse see Section 2

bench() ->
  Ls = [16, 32, 64, 128, 256, 512],
  N = 100,
  Bench = fun(L) ->
    S = lists:seq(1, L),
    Tn = time(N, fun() -> nreverse(S) end),
    Tr = time(N, fun() -> reverse(S) end),
    io:format("length: ~10w nrev: ~8w us rev: ~8w us~n", [L, Tn, Tr])
          end,
  lists:foreach(Bench, Ls).
time(N, F) ->
%% time in micro seconds
  T1 = erlang:system_time(micro_seconds),
  loop(N, F),
  T2 = erlang:system_time(micro_seconds),
  (T2 - T1).
loop(N, Fun) ->
  if N == 0 -> ok; true -> Fun(), loop(N - 1, Fun) end.

% With nreverse we have to traverse the list every time we append an element


%%%-------------------------------------------------------------------
%%% Section 6: Binary coding
%%%-------------------------------------------------------------------

intToBinA(Int) ->
  intToBinA(Int, []).
intToBinA(0, L) -> L;
intToBinA(Int, L) ->
  case Int rem 2 of
    0 -> intToBinA(Int div 2, [0 | L]);
    1 -> intToBinA(Int div 2, [1 | L])
  end.


intToBinB(Int) ->
  intToBinB(Int, [], greaterTwo(Int, 1)).
intToBinB(_, L, 0) ->
  reverse(L);
intToBinB(Int, L, N) ->
  case Int - N >= 0 of
    true -> intToBinB(Int - N, [1 | L], N div 2);
    false -> intToBinB(Int, [0 | L], N div 2)
  end.

greaterTwo(Int, Pow) ->
  case Pow > Int of
    true -> Pow div 2;
    false -> greaterTwo(Int, 2 * Pow)
  end.


%%%-------------------------------------------------------------------
%%% Section 7: Fibonacci
%%%-------------------------------------------------------------------

fib(0) -> 1;
fib(1) -> 1;
fib(N) ->
  fib(N - 1) + fib(N - 2).

fibb() ->
  Ls = [8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40],
  N = 10,
  Bench = fun(L) ->
    T = time(N, fun() -> fib(L) end),
    io:format("n: ~4w fib(n) calculated in: ~8w us~n", [L, T])
          end,
  lists:foreach(Bench, Ls).

% Computational complexity: O(2^n). We repeat the whole stack of computations
% at every recursive step.