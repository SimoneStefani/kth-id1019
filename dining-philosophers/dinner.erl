%%%-------------------------------------------------------------------
%%% @author Simone Stefani
%%% @copyright (C) 2017, simonestefani.me
%%% Created : 26. Jan 2017 20:32
%%%-------------------------------------------------------------------
-module(dinner).

-export([start/0]).

start() ->
  spawn(fun() -> init() end).

init() ->
  C1 = chopstick:start(),
  C2 = chopstick:start(),
  C3 = chopstick:start(),
  C4 = chopstick:start(),
  C5 = chopstick:start(),
  Ctrl = self(),
  philosopher:start(5, C1, C2, "Arendt", Ctrl, 1),
  philosopher:start(5, C2, C3, "Hypatia", Ctrl, 2),
  philosopher:start(5, C3, C4, "Simone", Ctrl, 3),
  philosopher:start(5, C4, C5, "Elizabeth", Ctrl, 4),
  philosopher:start(5, C5, C1, "Ayn", Ctrl, 5),
  wait(5, [C1, C2, C3, C4, C5]).

wait(0, Chopsticks) ->
  lists:foreach(fun(C) -> chopstick:quit(C) end, Chopsticks);
wait(N, Chopsticks) ->
  receive
    done ->
      wait(N - 1, Chopsticks);
    abort ->
      exit(abort)
  end.