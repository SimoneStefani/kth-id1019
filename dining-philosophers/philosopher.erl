%%%-------------------------------------------------------------------
%%% @author Simone Stefani
%%% @copyright (C) 2017, simonestefani.me
%%% Created : 26. Jan 2017 17:46
%%%-------------------------------------------------------------------
-module(philosopher).

-export([start/5]).


start(Hungry, Left, Right, Name, Ctrl) ->
  spawn_link(fun() -> init(Hungry, Left, Right, Name, Ctrl) end).


init(Hungry, Right, Left, Name, Ctrl) ->
  dreaming(Hungry, Right, Left, Name, Ctrl).

dreaming(0, _Left, _Right, Name, Ctrl) ->
  io:format("~s is done eating!~n", [Name]),
  Ctrl ! done;
dreaming(Hungry, Left, Right, Name, Ctrl) ->
  io:format("~s dreaming (aka thinking)...~n", [Name]),
  sleep(200, 200),
  waiting(Hungry, Left, Right, Name, Ctrl).


waiting(Hungry, Left, Right, Name, Ctrl) ->
  io:format("~s waiting for the chopsticks...~n",[Name]),
  
  case chopstick:request(Left) of
     ok ->
       case chopstick:request(Right) of
          ok ->
            io:format("~s could get both sticks!~n",[Name]),
            eating(Hungry, Left, Right, Name, Ctrl);
          no ->
            chopstick:return(Left),
            chopstick:return(Right),
            dreaming(Hungry, Left, Right, Name, Ctrl)
       end;
    no ->
      chopstick:return(Right),
      dreaming(Hungry, Left, Right, Name, Ctrl)
  end.


  eating(Hungry, Left, Right, Name, Ctrl) ->
    io:format("~s is eating! (~w)~n", [Name, Hungry - 1]),
    sleep(200, 200),
    chopstick:return(Left),
    chopstick:return(Right),
    dreaming(Hungry - 1, Left, Right, Name, Ctrl).


  sleep(T, D) ->
    timer:sleep(T + rand:uniform(D)).