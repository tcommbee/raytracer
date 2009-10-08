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
		dropwhile(
			fun({_, Intersection})->(vectorSqr(vectorSub(StartPos, Target, Intersection)) > 0) or (Intersection == undefined) end,
			sort(
				fun({_,_,A},{_,_,B}) -> A =< B end,
				intersections(World, StartPos, Target)
				)
			)
		)
	),
	if
		length =< 0 -> 0;
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

intersections([], _, _)->[];
intersections([First|RestOfWorld],StartPos,Target	)->[intersect(First, StartPos, Target)|intersections(RestOfWorld, StartPos, Target)].
	
%produce a raytraced image
%  Scene: a list of geometry and lightsources to be rendered
%  Dimensions: of the output image in px
%  Passes: the number of rays cast per px
%  returns: list of px values
trace(Scene, {width, height}, Passes) -> null.

vectorSqr(#coords{x=X, y=Y, z=Z}) -> X*X + Y*Y + Z*Z .
vectorMul(#coords{x=X, y=Y, z=Z}, #coords{x=A, y=B, z=C}) -> A*X + B*Y + C*Z .
scalarMul(#coords{x=X, y=Y, z=Z}, l) -> #coords{x=l*X, y=l*Y, z=l*Z} .
vectorAbs(V) -> math:sqrt( vectorSqr(V) ) .
vectorAdd(#coords{x=X, y=Y, z=Z}, #coords{x=A, y=B, z=C}) -> #coords{x=A+X, y=B+Y, z=C+Z} .
vectorSub(#coords{x=X, y=Y, z=Z}, #coords{x=A, y=B, z=C}) -> #coords{x=A-X, y=B-Y, z=C-Z} .

intersect(Object, StartPos, Target) ->
	CS = vectorSub(StartPos, Object#sphere.coords),
	ST = vectorSub(StartPos, Target),
	STsq = vectorSqr(ST),
	Left = vectorMul(CS, ST) / STsq,
	Right = (Left*Left) + (Object#sphere.radius-vectorsqr(CS))/STsq,

	SmT = vectorSub(StartPos, Target),
	SmC = vectorSub(StartPos, Object#sphere.coords),
	Square = vectorSqr(SmT),
	Left = vectorMul(SmT, SmC) / Square,
	Right =  (Object#squere.radius * Object#squere.radius) / Suare,
	if 
