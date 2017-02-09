%%%-------------------------------------------------------------------
%%% @author Simone Stefani
%%% @copyright (C) 2017, simonestefani.me
%%% Created : 26. Jan 2017 17:46
%%%-------------------------------------------------------------------
-module(chopstick).

-export([start/0, request/3, return/2, quit/1]).


% Start the available-gone process
start() ->
  Stick = spawn_link(fun() -> init() end),
  {stick, Stick}.

% Request a chopstick. The chopstick will change state from available
% to gone
request({stick, Pid}, Ref, After) ->
  Pid ! {request, Ref, self()},
  wait(Ref, After).

wait(Ref, After) ->
  receive 
    {granted, Ref} ->
      ok;
    {granted, _} ->
	    wait(Ref, After)
  after 
    After ->
	    no
  end.

% Request a chopstick. The chopstick will change state from gone to
% available
return({stick, Pid}, Ref) ->
    Pid ! {return, Ref}.

% Quit the state-machine
quit({stick, Pid}) ->
    Pid ! quit.


%%%-------------------------------------------------------------------
%%% Private functions
%%%-------------------------------------------------------------------

% Start process at entry-point available
init() ->
  available().

% The chopstick is available
available() ->
  receive
    {request, Ref, From} ->
	    From ! {granted, Ref},
	    gone(Ref);
    quit ->
      ok
  end.

% The chopstick is gone
gone(Ref) ->
  receive
    {return, Ref} ->
	    available();
	  quit ->
	    ok
  end.
