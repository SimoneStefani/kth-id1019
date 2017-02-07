%%%-------------------------------------------------------------------
%%% @author Simone Stefani
%%% @copyright (C) 2017, simonestefani.me
%%% Created : 18. Jan 2017 17:51
%%%-------------------------------------------------------------------
-module(env).

-export([new/0, add/3, lookup/2, closure/2]).


% Return an empty environment
new() ->
  [].

% Return an environment where the binding of the variable 
% Id to the structure Str has been added to the environment Env
add(Id, Str, Env) ->
  [{Id, Str} | Env].

% Return either {Id, Str}, if the variable Id is bound, or false
lookup(Id, Env) ->
  lists:keyfind(Id, 1, Env).