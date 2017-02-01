%%%-------------------------------------------------------------------
%%% @author Simone Stefani
%%% @copyright (C) 2017, simonestefani.me
%%% Created : 26. Jan 2017 17:46
%%%-------------------------------------------------------------------
-module(philosopher).

-export([start/5]).


start(Hungry, Left, Right, Name, Ctrl) ->
  spawn_link(fun() -> init(Hungry, Left, Right, Name, Ctrl) end).


% Helper function in order not to call one of the states directly from
% public function start/5
init(Hungry, Right, Left, Name, Ctrl) ->
  dreaming(Hungry, Right, Left, Name, Ctrl).


% The philosopher is dreaming.
dreaming(0, _Left, _Right, Name, Ctrl) ->
  io:format("~s is done eating!~n", [Name]),
  Ctrl ! done;
dreaming(Hungry, Left, Right, Name, Ctrl) ->
  io:format("~s is dreaming...~n", [Name]),
  sleep(200, 200),
  waiting(Hungry, Left, Right, Name, Ctrl).

% The philosopher is waiting to receive the chopsticks. If all
% chopsticks are in use, return to dreaming
waiting(Hungry, Left, Right, Name, Ctrl) ->
  io:format("~s waiting for the chopsticks...~n",[Name]),
  
  case chopstick:request(Left, 1000) of
     ok ->
       sleep(100, 100),
       case chopstick:request(Right, 1000) of
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


% The philosopher is eating. The chopsticks will be then returned
% and the hunger decreased by one
eating(Hungry, Left, Right, Name, Ctrl) ->
  io:format("~s is eating! (~w)~n", [Name, Hungry - 1]),
  sleep(200, 200),
  chopstick:return(Left),
  chopstick:return(Right),
  dreaming(Hungry - 1, Left, Right, Name, Ctrl).

% Helper method to randomise dreaming/eating time
sleep(T, D) ->
  timer:sleep(T + rand:uniform(D)).