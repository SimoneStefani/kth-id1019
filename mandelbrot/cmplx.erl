%%%-------------------------------------------------------------------
%%% @author Simone Stefani
%%% @copyright (C) 2017, simonestefani.me
%%% Created : 1. Feb 2017 22:06
%%%-------------------------------------------------------------------
-module(cmplx).

-compile(export_all).


new(X, Y) ->
  {X, Y}.

add({Ar, Ai}, {Br, Bi}) ->
  {Ar + Br, Ai + Bi}.

sqr({Ar, Ai}) ->
  {(Ar * Ar) - (Ai * Ai), 2 * Ar * Ai}.

abs({Ar, Ai}) ->
  math:sqrt(Ar * Ar + Ai * Ai).