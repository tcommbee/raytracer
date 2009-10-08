-module(raytracer).
-export([trace/3]).

-record(coords, {x = 0, y = 0, z = 0}).
-record(sphere, {radius = 1, coords = #coords{}, light = false}).

%cast a ray
%  World: a list of geometry and lightsources the ray is cast into
%  StartPos: current "position" of ray
%  Target: point the ray is directed at
%  returns: lightness as number
cast(World, StartPos, Target) ->
	{Obstacle, Impact, Length} = lists:last(
		lists:dropwhile(
			fun({_, Intersection, Y})->(Y > 0) or (Intersection == undefined) end,
			lists:sort(
				fun({_,_,A},{_,_,B}) -> A =< B end,
				intersections(World, StartPos, Target)
			)
		)
	),
	if
		Length =< 0 -> 0;
		Obstacle#sphere.light == false -> 0.6 * cast(World, Impact, reflect(Obstacle, StartPos, Impact));
		Obstacle#sphere.light == true  -> LightSource = Obstacle, lightness(LightSource, StartPos, Impact);
		true -> 0
	end
.

%assumes that vector intersect LightSource!
lightness(LightSource, StartPos, Impact) ->
	MinDistanceToCenter = vectorMul(LightSource#sphere.coords - StartPos, Impact - StartPos)/vectorAbs(Impact - startPos),
	LightIntensity = 1000,
	LightIntensity * math:sqrt(math:sqr(LightSource#sphere.radius) - math:sqr(MinDistanceToCenter)).

reflect(#sphere{radius = R, coords = C, light = false}, StartPos, Impact) ->
	A = Impact - StartPos,
	D = Impact - C,
	B = scalarMul(vectorMul(A, D), -2 / R),
	Impact + B + A.

intersections([], _, _)->[];
intersections([First|RestOfWorld],StartPos,Target	)->[intersect(First, StartPos, Target)|intersections(RestOfWorld, StartPos, Target)].
	
%produce a raytraced image
%  Scene: a list of geometry and lightsources to be rendered
%  Dimensions: of the output image in px
%  Passes: the number of rays cast per px
%  returns: list of px values
trace(Scene, {Width, Height}, Passes) ->
	lists:map(
		fun(A)->
			cast(Scene, #coords{x=0, y=0, z=0}, A) end,
			lists:map(
				fun(X) -> {-Width/2+X rem Width, -Height/2+X div Height} end,
				lists:seq(0, Width*Height)
			)
	)
.

vectorSqr(#coords{x=X, y=Y, z=Z}) -> X*X + Y*Y + Z*Z .
vectorMul(#coords{x=X, y=Y, z=Z}, #coords{x=A, y=B, z=C}) -> A*X + B*Y + C*Z .
scalarMul(#coords{x=X, y=Y, z=Z}, l) -> #coords{x=l*X, y=l*Y, z=l*Z} .
vectorAbs(V) -> math:sqrt( vectorSqr(V) ) .
vectorAdd(#coords{x=X, y=Y, z=Z}, #coords{x=A, y=B, z=C}) -> #coords{x=A+X, y=B+Y, z=C+Z} .
vectorSub(#coords{x=X, y=Y, z=Z}, #coords{x=A, y=B, z=C}) -> #coords{x=A-X, y=B-Y, z=C-Z} .

intersect(Object, StartPos, Target) ->
	CS = vectorSub(StartPos, Object#sphere.coords),
	ST = vectorSub(StartPos, Target),
	R2 = Object#sphere.radius * Object#sphere.radius,
	
	ST2 = vectorSqr(ST),
	CS2 = vectorSqr(CS),
	CStST = vectorMul(CS, ST),
	
	Left = - CStST / ST2,
	Right = (Left*Left) - (CS2/ST2) + R2,
	
	L = if
		Right  < 0        -> undefined;
		Right == 0        -> Left;
		Left - Right =< 0 -> Left + Right;
		true              -> Right - Left
	end,
	
	if
		(L == undefined) or (L =< 0)  -> undefined;
		true -> vectorAdd(scalarMul(-L, ST), StartPos)
	end
.
