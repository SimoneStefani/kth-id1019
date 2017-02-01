%%%-------------------------------------------------------------------
%%% @author Simone Stefani
%%% @copyright (C) 2017, simonestefani.me
%%% Created : 26. Jan 2017 17:46
%%%-------------------------------------------------------------------
-module(chopstick).

-export([start/0, request/1, return/1, quit/1]).


% Start the available-gone process
start() ->
    spawn_link(fun() -> init() end).

% Request a chopstick. The chopstick will change state from available
% to gone
request(Stick) ->
    Stick ! {request, self()},
    receive
        granted ->
            ok
    end.

% Request a chopstick. The chopstick will change state from gone to
% available
return(Stick) ->
    Stick ! {return}.

% Quit the state-machine
quit(Stick) ->
    Stick ! {quit}.


%%%-------------------------------------------------------------------
%%% Private functions
%%%-------------------------------------------------------------------

% Start process at entry-point available
init() ->
    available().

% The chopstick is available
available() ->
    receive
        {request, From} ->
            From ! granted,
            gone();
        quit ->
            ok
    end.

% The chopstick is gone
gone() ->
    receive
        return ->
            available;
        quit ->
            ok
    end
