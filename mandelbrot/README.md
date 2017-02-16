*Programming II (ID1019) - Royal Institute of Technology KTH*
# Mandelbrot Fractals

### 1. Introduction
The **Mandelbrot** set is the set of complex numbers <img src="http://mathurl.com/jnjbf2u.png"> for which the function <img src="http://mathurl.com/hczw332.png"> does not diverge when iterated from <img src="http://mathurl.com/h4urouj.png">. By iterating the formula for each point of the complex plane, it is thus possible to divide the points into two categories: points that belong to the Mandelbrot set and points that do not. The <img src="http://mathurl.com/h5spwnl.png"> values ​​obtained by the iteration process describe a path called **"orbit"** on the complex plane. It is possible to imagine a calculation procedure to determine the membership of a point <img src="http://mathurl.com/jnjbf2u.png"> to the set as the construction of the orbit of <img src="http://mathurl.com/jnjbf2u.png">. If the orbit goes to infinity it means that the series diverges and therefore <img src="http://mathurl.com/jnjbf2u.png"> does not belong to the set. If the orbit does not go to infinity it means that the series does not diverge and <img src="http://mathurl.com/jnjbf2u.png"> belongs to the set. 

It is obviously impossible to calculate a complete orbit because it is formed by an infinite number of points <img src="http://mathurl.com/h5spwnl.png"> and so would require an infinite number of iterations to be computed. Fortunately, it is known that if the absolute value of <img src="http://mathurl.com/h5spwnl.png"> becomes greater than 2, then the orbit diverges. Thanks to this result, it is possible to recognize the points which do not belong to group because their orbit, after a number of iterations, comes out of the circle of radius 2. Unfortunately a very high number of iterations may be necessary before the orbit of a point not belonging to the set leaves the circle. For this reason, it is common practice to set a maximum number of iterations, after which it is assumed that if the orbit is not yet out of the circle, then it will stay in the circle and then the point in question belongs to the Mandelbrot set.


### 2. Complex numbers

The first module in this project is called *cmplx* and handles basic operations with complex numbers in the form <img src="http://mathurl.com/hvkykm8.png">.

```erlang
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
```


### 3. Belonging to the set

The module *brot* determines if a number <img src="http://mathurl.com/jnjbf2u.png"> belongs to the Mandelbrot set. Given a complex number <img src="http://mathurl.com/jnjbf2u.png"> and a maximum number of iterations *m* (depth), the function `mandelbrot/2` return the value *i* at which
<img src="http://mathurl.com/j86mkmm.png"> or *0* if it does not for any <img src="http://mathurl.com/hkcq8qa.png">.

```erlang
mandelbrot(C, M) ->
  Z0 = cmplx:new(0, 0),
  I = 0,
  test(I, Z0, C, M).


test(I, _, _, I)->
	0;
test(I, Z0, C, M) ->
  Zabs = cmplx:abs(Z0),
  if
    Zabs >= 2 -> I;
    true -> test(I + 1, cmplx:add(C, cmplx:sqr(Z0)), C, M)
  end.
```


### 4. Coloring the fractal
The next step is to determine a color schema for the Mandelbrot fractal. One common method is to assign to a point a color that depends on the number of iterations necessary to determine if the points belongs to the set. The `convert/2` function accepts a *depth* and a *maximum depth* of a point and determines the color for such point.

Here is an example of a color mapping function:

```erlang
convert(Depth, Max) ->
  F = Depth / Max,
  A = F * 4,
  X = trunc(A),
  Y = trunc(255 * (A - X)),
  case X of
    0 -> {Y, 0, 0};
    1 -> {255, Y, 0};
    2 -> {255 - Y, 255, 0};
    3 -> {0, 255, Y };
    4 -> {0, 255 - Y, 255} 
  end.
```


### 5. Computing the set

The module *mandel* is responsible for computing the Mandelbrot set. The function `mandelbrot/6` takes as arguments *height, width, upper left corner coordinate, offset* and *depth*, and returns a list of lists with RGB tuples as elements.

The function
```erlang
Trans = fun(W, H) -> cmplx:new(X + K * (W - 1), Y - K * (H - 1)) end.
```
transform a pixel of coordinates <img src="http://mathurl.com/hrr56u7.png"> into the corresponding complex number of the form <img src="http://mathurl.com/hvkykm8.png">. The functions `rows/5` and `row/5` help to iterate through all rows and columns of the image and generate the pixels with the right color.

```erlang
mandelbrot(Width, Height, X, Y, K, Depth) ->
	Trans = fun(W, H) ->
			cmplx:new(X + K * (W - 1), Y - K * (H - 1))
		end,
	rows(Width, Height, Trans, Depth, []).

rows(_, 0, _, _, Rows)->
  	Rows;
rows(Width, Height, Trans, Depth, Rows)->
	Row = row(Width, Height, Trans, Depth, []),
	rows(Width, Height - 1, Trans, Depth, [Row | Rows]).

row(0, _, _, _, Row)->
	Row;
row(Width, Height, Trans, Depth, Row)->
	DepthI = brot:mandelbrot(Trans(Width, Height), Depth),
	Pixel = color:convert(DepthI, Depth),
	row(Width - 1, Height, Trans, Depth, [Pixel | Row]). 
```
