%%%-------------------------------------------------------------------
%%% @author Simone Stefani
%%% @copyright (C) 2017, simonestefani.me
%%% Created : 1. Feb 2017 23:25
%%%-------------------------------------------------------------------
-module(mandel).

-export([demo/0, mandelbrot/6]).


% Generate a Mandelbrot fractal image
mandelbrot(Width, Height, X, Y, K, Depth) ->
	Trans = fun(W, H) ->
			cmplx:new(X + K * (W - 1), Y - K * (H - 1))
		end,
	rows(Width, Height, Trans, Depth, []).


% Generate rows of image
rows(_, 0, _, _, Rows)->
  	Rows;
rows(Width, Height, Trans, Depth, Rows)->
	Row = row(Width, Height, Trans, Depth, []),
	rows(Width, Height - 1, Trans, Depth, [Row | Rows]).

% Generate pixels in row
row(0, _, _, _, Row)->
	Row;
row(Width, Height, Trans, Depth, Row)->
	DepthI = brot:mandelbrot(Trans(Width, Height), Depth),
	Pixel = color:convert(DepthI, Depth),
	row(Width - 1, Height, Trans, Depth, [Pixel | Row]).


% Test the Mandelbrot generator with a small set	
demo() ->
	small(-2.6, 1.2, 1.6).
small(X, Y, X1) ->
	Width = 960,
	Height = 540,
	K = (X1 - X) / Width,
	Depth = 64,
	T0 = now(),
	Image = mandelbrot(Width, Height, X, Y, K, Depth),
	T = timer:now_diff(now(), T0),
	io:format("picture generated in ~w ms~n", [T div 1000]),
	ppm:write("small.ppm", Image).