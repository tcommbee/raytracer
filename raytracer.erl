-module(raytracer).
-export([trace/3, traceToFile/4, test/1]).

-record(coords, {x = 0, y = 0, z = 0}).
-record(sphere, {radius = 1, coords = #coords{}, light = false}).

%cast a ray
%  World: a list of geometry and lightsources the ray is cast into
%  StartPos: current "position" of ray
%  Target: point the ray is directed at
%  returns: lightness as number
cast([], _, _) -> 0;
cast(World, StartPos, Target) ->
	{Obstacle, Impact, _} = chooseClosest(
			lists:sort(
				fun({_,_,A},{_,_,B}) -> A =< B end,
				lists:filter(
					fun({_, Intersection, Y})->(Y > 0) and is_record(Intersection,coords) end,
				intersections(World, StartPos, Target)
			)
		)
	),
	if
		not is_record(Impact, coords) -> 0;
		Obstacle#sphere.light == false -> 0.6 * cast(World, Impact, reflect(Obstacle, StartPos, Impact));
		Obstacle#sphere.light == true  -> LightSource = Obstacle, lightness(LightSource, StartPos, Impact);
		true -> 0
	end
.

chooseClosest([]) -> {undefined, undefined, undefined};
chooseClosest([H|_]) -> H.

%assumes that vector intersect LightSource!
lightness(LightSource, StartPos, Impact) ->
	MinDistanceToCenter = vectorMul(LightSource#sphere.coords - StartPos, Impact - StartPos)/vectorAbs(Impact - startPos),
	LightIntensity = 1000,
	LightIntensity * math:sqrt(math:sqr(LightSource#sphere.radius) - math:sqr(MinDistanceToCenter)).

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


trace_dummy(Scene, _, _) -> Scene.


traceToFile(File, Scene, {Width, Height}, Passes) ->
        Picture = trace_dummy(Scene, {Width, Height}, Passes),
        Out = prep([Width, Height, 255] ++ Picture),
        case file:open(File, [write]) of
                {ok, Fd} ->
                        file:write(Fd, ["P2\n", "# Erlang Raytracer Output\n"] ++ Out),
                        file:close(Fd);
                {error, R} ->
                        exit(R)
        end.

prep(List) ->
        lists:map(fun(E) -> integer_to_list(E) ++ ["\n"] end, List).

vectorSqr(#coords{x=X, y=Y, z=Z}) -> X*X + Y*Y + Z*Z .
vectorMul(#coords{x=X, y=Y, z=Z}, #coords{x=A, y=B, z=C}) -> A*X + B*Y + C*Z .
scalarMul(#coords{x=X, y=Y, z=Z}, L) -> #coords{x=L*X, y=L*Y, z=L*Z} .
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
		(L == undefined) or (L =< 0)  -> {Object, undefined, -1};
		true -> {Object, vectorAdd(scalarMul(ST, -L), StartPos), L}
	end
.

test(X) ->
	trace(
		[
			#sphere{radius=50, coords = #coords{x=0,y=0,z=-150}}
		],
		{X,X},
		1
	)
.
