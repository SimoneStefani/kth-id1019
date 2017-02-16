%%%-------------------------------------------------------------------
%%% @author Simone Stefani
%%% @copyright (C) 2017, simonestefani.me
%%% Created : 1. Feb 2017 22:06
%%%-------------------------------------------------------------------
-module(cmplx).

-compile(export_all).


%%%-------------------------------------------------------------------
%%% Basic operations with complex numbers
%%%-------------------------------------------------------------------

% Generate a new complex number of the form X + Yi
new(X, Y) ->
  {X, Y}.

% Add two complex numbers: (A + Bi) + (C + Di) = (A + C) + (Bi + Di)
add({A, B}, {C, D}) ->
  {A + C, B + D}.

% Square a complex number: (A + Bi)^2 = (A^2 - (B)^2) + 2A(Bi)
sqr({A, B}) ->
  {(A * A) - (B * B), 2 * A * B}.

% Absolute value of a complex number: |A + Bi| = √(A^2 + B^2)
abs({A, B}) ->
  math:sqrt(A * A + B * B).