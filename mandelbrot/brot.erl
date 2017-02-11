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
test(_,_,_,0)->
	0;
test(I, Z0, C, M) ->
  Zsqrd = cmplx:sqr(Z0),
  ZplusOne = cmplx:add(C, Zsqrd),
  Zabs = cmplx:abs(Z0),
  if
    Zabs >= 2 ->
      I;
    true ->
      test(I + 1, ZplusOne, C, M - 1)
  end.