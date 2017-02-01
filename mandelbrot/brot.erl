%%%-------------------------------------------------------------------
%%% @author Simone Stefani
%%% @copyright (C) 2017, simonestefani.me
%%% Created : 1. Feb 2017 22:33
%%%-------------------------------------------------------------------
-module(brot).

-compile(export_all).


mandelbrot({Cr, Ci}, M) ->
  Z0 = cmplx:new(Cr, Ci),
  I = 0,
  test(I, Z0, {Cr, Ci}, M).

test(I, Z0, C, M) ->
  case (I < M) and (cmplx:abs(Z0) < 2.0) of
    true ->
      Zsqrd = cmplx:sqr(Z0),
      ZplusOne = cmplx:add(C, Zsqrd),
      test(I + 1, ZplusOne, C, M);
    false ->
      I
  end.