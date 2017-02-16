%%%-------------------------------------------------------------------
%%% @author Simone Stefani
%%% @copyright (C) 2017, simonestefani.me
%%% Created : 1. Feb 2017 22:33
%%%-------------------------------------------------------------------
-module(brot).

-export([mandelbrot/2]).


mandelbrot(C, M) ->
  Z0 = cmplx:new(0, 0),
  I = 0,
  test(I, Z0, C, M).


% Test if reached max number of iterations and return 0. If the 
% absolute value of Z is greater or equal than 2 return I
test(I, _, _, I)->
	0;
test(I, Z0, C, M) ->
  Zabs = cmplx:abs(Z0),
  if
    Zabs > 2 ->
      I;
    true ->
      test(I + 1, cmplx:add(C, cmplx:sqr(Z0)), C, M)
  end.