-module(raytracer).
-export([trace/3, traceToFile/4, test/1,intersect/3]).

-record(color, {r = 0, g = 0, b = 0}).
-record(coords, {x = 0, y = 0, z = 0}).
-record(sphere, {radius = 1, coords = #coords{}, light = false, color = #color{}}).

%cast a ray
%  World: a list of geometry and lightsources the ray is cast into
%  StartPos: current "position" of ray
%  Target: point the ray is directed at
%  Depth: shall be 1 in first step
%  returns: lightness as number
cast(World, Target, StartPos, Depth) ->
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
		not is_record(Impact, coords) and depth > 0 -> #color{r = 15, g = 15, b = 15};
		not is_record(Impact, coords)-> #color{r = 15, g = 15, b = 15};
		Obstacle#sphere.light == false ->
			colorMul(Obstacle#sphere.color, cast(World, Impact, reflect(Obstacle, StartPos, Impact), Depth+1));
		Obstacle#sphere.light == true -> LightSource = Obstacle, lightness(LightSource, StartPos, Impact, ShortestPoint)
	end
.

cast(World, Target) ->
	cast(World, #coords{x=0, y=0, z=0}, Target, 0) .

chooseClosest([]) -> {undefined, undefined, undefined, undefined};
chooseClosest([H|_]) -> H.

%assumes that vector intersect LightSource!
lightness(LightSource, StartPos, _, ShortestPoint) ->
	A = vectorAdd(ShortestPoint, StartPos),
	MinDistanceToCenter = vectorAbs(vectorSub(LightSource#sphere.coords, A)),
	LightIntensity = LightSource#sphere.color,
	R = LightSource#sphere.radius,
	colorScale(LightIntensity, math:sqrt(R*R - MinDistanceToCenter*MinDistanceToCenter)/R).

reflect(#sphere{radius = R, coords = C, light = false}, StartPos, Impact) ->
	A = vectorSub(Impact, StartPos),
	D = vectorSub(Impact, C),
	Distance = abs(vectorMul(A, D)),
	B = scalarMul(D, (Distance * 2) / (R*R)),
	vectorAdd(vectorAdd(Impact, B), A).

intersections([], _, _) -> [];
intersections([First|RestOfWorld], StartPos, Target) -> [intersect(First, StartPos, Target) | intersections(RestOfWorld, StartPos, Target)].

%traceWorker rendered a range of pixels ("worker thread")
%  Main: PID of process to receive the rendered pixels
%  ThreadServer: PID of process to ask for pixels to render
traceWorker(Main, ThreadServer) ->
	receive
		{ done } ->
			ok
	after 0 ->
		ThreadServer ! { getJob, self() },
		receive
			{ done } ->
				ok;
			{ job, Index, Amount, Scene, CanvasSize, Width, Height } ->
				io:format("traceWorker() PID ~w, Index ~w~n", [self(), Index]),
				Upper = if
					Index+Amount >= CanvasSize -> CanvasSize-1;
					true -> Index+Amount-1
				end,
				Values = lists:map(
					fun(At) ->
						Pixel = #coords{x=-Width/2+At rem Width, y=-Height/2+At div Height, z=-300},
						cast(Scene, Pixel)
					end,
					lists:seq(Index, Upper)
				),
				Main ! { pixels, Index, Amount, Values },
				traceWorker(Main, ThreadServer)
		end
	end
.

%threadServer anwers @traceWorker/2's request for pixels to process
%  Index: current index (begin with 0)
%  PixelsAtOnce: pixels to serve at once ...
%  Scene: scene to render
%  CanvasSize: height * width
threadServer(Index, _, _, CanvasSize, _, _) when Index >= CanvasSize ->
	receive {done} -> ok end;
threadServer(Index, PixelsAtOnce, Scene, CanvasSize, Width, Height) ->
	receive
		{getJob, Caller} ->
			Caller ! { job, Index, PixelsAtOnce, Scene, CanvasSize, Width, Height },
			threadServer(Index+PixelsAtOnce, PixelsAtOnce, Scene, CanvasSize, Width, Height)
	end
.

%collect collects @traceWorker/2's results
%  Index: current index (begin with 0)
%  CanvasSize: height * width
%  List: already collected pixels
collect(Index, CanvasSize, List) when Index >= CanvasSize -> List;
collect(Index, CanvasSize, List) ->
	receive
		{ pixels, Index, Amount, Values } ->
			collect(Index + Amount, CanvasSize, List ++ Values)
	end
.

%produce a raytraced image
%  Scene: a list of geometry and lightsources to be rendered
%  Dimensions: of the output image in px
%  Passes: the number of rays cast per px
%  returns: list of px values
trace(Scene, {Width, Height}, 1) ->
	PIXELS_AT_ONCE = 8096,
	THREAD_COUNT = 8,
	
	Main = self(),
	ThreadServer = spawn(fun() -> threadServer(0, PIXELS_AT_ONCE, Scene, Width*Height, Width, Height) end),
	Threads = lists:map(
		fun(_) -> spawn(fun() -> traceWorker(Main, ThreadServer) end) end,
		lists:seq(0, THREAD_COUNT-1)
	),
	Result = collect(0, Width*Height, []),
	lists:map(
		fun(Thread) -> Thread ! {done} end,
		Threads
	),
	ThreadServer ! {done},
	Result
.

traceToFile(File, Scene, {Width, Height}, Passes) ->
	Picture = trace(Scene, {Width, Height}, Passes),
	Out = prep([Width, Height, 255] ++ lists:flatten(lists:map(fun colorToList/1, Picture))),
	case file:open(File, [write]) of
		{ok, Fd} ->
			file:write(Fd, ["P3\n", "# Erlang Raytracer Output\n"] ++ Out),
			file:close(Fd);
		{error, R} ->
			exit(R)
	end
.

colorToList(#color{r=R, g=G, b=B}) -> [R,G,B].

prep(List) ->
	lists:map(fun(R) -> integer_to_list(trunc(R)) ++ ["\n"] end, List).

vectorSqr(#coords{x=X, y=Y, z=Z}) -> (X*X + Y*Y + Z*Z).
vectorMul(#coords{x=X, y=Y, z=Z}, #coords{x=A, y=B, z=C}) -> A*X + B*Y + C*Z .
scalarMul(#coords{x=X, y=Y, z=Z}, L) -> #coords{x=L*X, y=L*Y, z=L*Z} .
vectorAbs(V) -> math:sqrt( vectorSqr(V) ) .
vectorAdd(#coords{x=X, y=Y, z=Z}, #coords{x=A, y=B, z=C}) -> #coords{x=A+X, y=B+Y, z=C+Z} .
vectorSub(#coords{x=X, y=Y, z=Z}, #coords{x=A, y=B, z=C}) -> #coords{x=X-A, y=Y-B, z=Z-C} .

colorMul(#color{r=R1,g=G1,b=B1}, #color{r=R2,g=G2,b=B2}) ->
	#color{r=R1*R2, g=G1*G2, b=B1*B2}.
colorScale(#color{r=R,g=G,b=B}, S) ->
	#color{r=R*S, g=G*S, b=B*S}.

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
			#sphere{radius=3000, coords = #coords{x=0,y=0,z=-8000}, light = false, color = #color{r=1,g=1,b=1}},
			#sphere{radius=600, coords = #coords{x=-2000,y=-2200,z=-6000}, light = true, color = #color{r=255,g=255,b=255}},
			#sphere{radius=600, coords = #coords{x=0,y=-2200,z=-5000}, light = false, color = #color{r=1,g=1,b=1}},
			#sphere{radius=600, coords = #coords{x=2000,y=-2200,z=-4000}, light = true, color = #color{r=255,g=0,b=0}},
			#sphere{radius=600, coords = #coords{x=-2000,y=0,z=-5000}, light = true, color = #color{r=100,g=0,b=255}},
			#sphere{radius=600, coords = #coords{x=2000,y=-0,z=-5000}, light = true, color = #color{r=255,g=255,b=0}},
			#sphere{radius=600, coords = #coords{x=-2000,y=2200,z=-6000}, light = true, color = #color{r=128,g=255,b=0}},
			#sphere{radius=600, coords = #coords{x=0,y=2200,z=-5000}, light = false, color = #color{r=1,g=1,b=1}},
			#sphere{radius=600, coords = #coords{x=2000,y=2200,z=-4000}, light = true, color = #color{r=0,g=0,b=255}}
		],
		{X,X},
		1
	)
.
