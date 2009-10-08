-module(raytracer).
-export([trace/3, traceToFile/4, test/1,intersect/3]).

-record(coords, {x = 0, y = 0, z = 0}).
-record(sphere, {radius = 1, coords = #coords{}, light = false}).

%cast a ray
%  World: a list of geometry and lightsources the ray is cast into
%  StartPos: current "position" of ray
%  Target: point the ray is directed at
%  returns: lightness as number
cast([], _, _) -> 0;
cast(World, StartPos, Target) ->
	{Obstacle, Impact, _, ShortestPoint} = chooseClosest(
			lists:sort(
				fun({_,_,A,_},{_,_,B,_}) -> A =< B end,
				lists:filter(
					fun({_, Intersection, Y, _})->(Y > 0) and is_record(Intersection,coords) end,
					intersections(World, StartPos, Target)
				)
			)
	),
	if
		not is_record(Impact, coords) -> 22;
		Obstacle#sphere.light == false -> 0.6 * cast(World, Impact, reflect(Obstacle, StartPos, Impact));
		Obstacle#sphere.light == true  -> LightSource = Obstacle, lightness(LightSource, StartPos, Impact, ShortestPoint);
		true -> 33
	end
.

chooseClosest([]) -> {undefined, undefined, undefined, undefined};
chooseClosest([H|_]) -> H.

%assumes that vector intersect LightSource!
lightness(LightSource, StartPos, Impact, ShortestPoint) ->
	MinDistanceToCenter = vectorAbs(vectorSub(LightSource#sphere.coords, ShortestPoint)),
	LightIntensity = 255,
	R = LightSource#sphere.radius,
	LightIntensity * math:sqrt(R*R - MinDistanceToCenter*MinDistanceToCenter)/R.
	%1000.

reflect(#sphere{radius = R, coords = C, light = false}, StartPos, Impact) ->
	A = vectorSub(Impact, StartPos),
	D = vectorSub(Impact, C),
	B = scalarMul(D, vectorMul(A, D) * (-2) / R),
	vectorAdd(vectorAdd(Impact, B), A).

intersections([], _, _)->[];
intersections([First|RestOfWorld],StartPos,Target	)->[intersect(First, StartPos, Target)|intersections(RestOfWorld, StartPos, Target)].
	
%produce a raytraced image
%  Scene: a list of geometry and lightsources to be rendered
%  Dimensions: of the output image in px
%  Passes: the number of rays cast per px
%  returns: list of px values
trace(Scene, {Width, Height}, 1) ->
	lists:map(
		fun(A)->
			cast(Scene, #coords{x=0, y=0, z=0}, A) end,
			lists:map(
				fun(X) -> #coords{x=-Width/2+X rem Width, y=-Height/2+X div Height, z=-300} end,
				lists:seq(0, Width*Height)
			)
	)
.


traceToFile(File, Scene, {Width, Height}, Passes) ->
        Picture = trace(Scene, {Width, Height}, Passes),
        Out = prep([Width, Height, 1000] ++ Picture),
        case file:open(File, [write]) of
                {ok, Fd} ->
                        file:write(Fd, ["P2\n", "# Erlang Raytracer Output\n"] ++ Out),
                        file:close(Fd);
                {error, R} ->
                        exit(R)
        end.

prep(List) ->
        lists:map(fun(E) -> integer_to_list(trunc(E)) ++ ["\n"] end, List).

vectorSqr(#coords{x=X, y=Y, z=Z}) -> (X*X + Y*Y + Z*Z).
vectorMul(#coords{x=X, y=Y, z=Z}, #coords{x=A, y=B, z=C}) -> A*X + B*Y + C*Z .
scalarMul(#coords{x=X, y=Y, z=Z}, L) -> #coords{x=L*X, y=L*Y, z=L*Z} .
vectorAbs(V) -> math:sqrt( vectorSqr(V) ) .
vectorAdd(#coords{x=X, y=Y, z=Z}, #coords{x=A, y=B, z=C}) -> #coords{x=A+X, y=B+Y, z=C+Z} .
vectorSub(#coords{x=X, y=Y, z=Z}, #coords{x=A, y=B, z=C}) -> #coords{x=X-A, y=Y-B, z=Z-C} .

intersect(Object, StartPos, Target) ->
	CS = vectorSub(Object#sphere.coords, StartPos),
	ST = vectorSub(StartPos, Target),
	R2 = Object#sphere.radius * Object#sphere.radius,
	
	
	ST2 = vectorSqr(ST),
	CS2 = vectorSqr(CS),
	CStST = vectorMul(CS, ST),
	
	
	Left = - (CStST / ST2),
	Right = (Left*Left) + ((R2-CS2)/ST2),
	
	
	L = if
		Right  < 0        -> undefined;
		true              -> Left-math:sqrt(Right)
	end,
	if
		(L == undefined) -> {Object, undefined, L, undefined};
		true -> {Object, vectorAdd(scalarMul(ST, -L), StartPos), L, scalarMul(ST, -Left)}
	end
.

test(X) ->
	traceToFile("X.pnm",
		[
			#sphere{radius=2000, coords = #coords{x=0,y=0,z=-120000}, light = true},
			#sphere{radius=2000, coords = #coords{x=0,y=2000,z=-120000}, light = true},
			#sphere{radius=99999, coords = #coords{x=0,y=-120000,z=0}, light = true},
			#sphere{radius=99999, coords = #coords{x=0,y=120000,z=0}, light = true},
			#sphere{radius=99999, coords = #coords{x=-120000,y=0,z=0}, light = true},
			#sphere{radius=99999, coords = #coords{x=120000,y=0,z=0}, light = true}
		],
		{X,X},
		1
	)
.
