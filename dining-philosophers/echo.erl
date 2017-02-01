%%%-------------------------------------------------------------------
%%% @author Simone Stefani
%%% @copyright (C) 2017, simonestefani.me
%%% Created : 26. Jan 2017 17:08
%%%-------------------------------------------------------------------
-module(echo).

-export([go/0, loop/0]).

% Send message to other process and wait for echoed answer
go() ->
  Pid = spawn(echo, loop, []),
  Pid ! {self(), hello},
  receive
    {Pid, Msg} ->
      io:format("~w~n", [Msg])
  end,
  Pid ! stop.

% Respond to parent process echoing the received message
loop() ->
  receive
    {From, Msg} ->
      From ! {self(), Msg},
      loop();
    stop ->
      true
  end.