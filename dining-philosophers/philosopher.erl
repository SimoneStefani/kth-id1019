%%%-------------------------------------------------------------------
%%% @author Simone Stefani
%%% @copyright (C) 2017, simonestefani.me
%%% Created : 26. Jan 2017 17:46
%%%-------------------------------------------------------------------
-module(philosopher).

-export([start/6]).

-define(Dream, 400).
-define(Eat, 400).
-define(Delay, 40).
-define(Timeout, 1000).


start(Hungry, Left, Right, Name, Ctrl, Seed) ->
  spawn_link(fun() -> init(Hungry, Left, Right, Name, Ctrl, Seed) end).


% Helper function in order not to call one of the states directly from
% public function start/5.
init(Hungry, Right, Left, Name, Ctrl, Seed) ->
  random:seed(Seed, Seed, Seed),
  dreaming(Hungry, Right, Left, Name, Ctrl).


% The philosopher is dreaming.
dreaming(0, _Left, _Right, Name, Ctrl) ->
  io:format("~s is done eating!~n", [Name]),
  Ctrl ! done;
dreaming(Hungry, Left, Right, Name, Ctrl) ->
  io:format("~s is dreaming...~n", [Name]),
  delay(?Dream),
  waiting(Hungry, Left, Right, Name, Ctrl).

% The philosopher is waiting to receive the chopsticks. If all
% chopsticks are in use, return to dreaming
waiting(Hungry, Left, Right, Name, Ctrl) ->
  io:format("~s waiting for the chopsticks...~n",[Name]),

  Ref = make_ref(),
  case chopstick:request(Left, Ref, ?Timeout) of
     ok ->
       delay(?Delay),
       case chopstick:request(Right, Ref, ?Timeout) of
          ok ->
            io:format("~s could get both sticks!~n",[Name]),
            eating(Hungry, Left, Right, Name, Ctrl, Ref);
          no ->
            chopstick:return(Left, Ref),
            chopstick:return(Right, Ref),
            dreaming(Hungry, Left, Right, Name, Ctrl)
       end;
    no ->
      chopstick:return(Left, Ref),
      dreaming(Hungry, Left, Right, Name, Ctrl)
  end.


% The philosopher is eating. The chopsticks will be then returned
% and the hunger decreased by one
eating(Hungry, Left, Right, Name, Ctrl, Ref) ->
  io:format("~s is eating! (~w)~n", [Name, Hungry - 1]),
  delay(?Eat),
  chopstick:return(Left, Ref),
  chopstick:return(Right, Ref),
  dreaming(Hungry - 1, Left, Right, Name, Ctrl).

% Helper method to randomise dreaming/eating time
delay(T) ->
    sleep(T).

sleep(0) -> ok; 
sleep(T) -> timer:sleep(rand:uniform(T)).