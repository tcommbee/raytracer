-module(raytracer).
-export([trace/3, traceToFile/4, test/1,intersect/3]).

-record(color, {r = 0, g = 0, b = 0}).
-record(coords, {x = 0, y = 0, z = 0}).
-record(sphere, {radius = 1, coords = #coords{}, light = false, color = #color{}}).

-define(GLOBAL_ILLUMINATION, true).
-define(PIXELS_AT_ONCE, 16384).
-define(THREAD_COUNT, 8).

%cast a ray
%  World: a list of geometry and lightsources the ray is cast into
%  Target: point the ray is directed at
%  StartPos: current "position" of ray
%  returns: lightness as number
cast(World, Target, StartPos) ->
	Ray = emitRay(World, Target, StartPos, 0, []),
	case Ray of
		[false] ->
			#color{};
		[true|[Color|Colors]] ->
			lists:foldl(fun colorMul/2, Color, Colors);
		[_|Colors] when ?GLOBAL_ILLUMINATION ->
			lists:foldl(fun colorMul/2, #color{r=128, g=128, b=128}, Colors);
		_ ->
			#color{}
	end
.

emitRay(World, Target, StartPos, Depth, Trace) ->
	{Obstacle, Impact, _, ShortestPoint} = chooseClosest(
		lists:sort(
			fun({_,_,A,_},{_,_,B,_}) -> A =< B end,
			lists:filter(
				fun({_, Intersection, Y, _}) -> (Y > 0) and is_record(Intersection,coords) end,
				intersections(World, StartPos, Target)
			)
		)
	),
	if
		not is_record(Impact, coords) ->
			[false|Trace];
		Obstacle#sphere.light == false ->
			Reflection = reflect(Obstacle, StartPos, Impact),
			emitRay(World, Reflection, Impact, Depth+1, [Obstacle#sphere.color|Trace]);
		true ->
			[ true| [lightness(Obstacle, StartPos, Impact, ShortestPoint)|Trace] ]
	end
.

chooseClosest([]) -> {undefined, undefined, undefined, undefined};
chooseClosest([H|_]) -> H.

%assumes that vector intersects LightSource!
lightness(LightSource, StartPos, Impact, ShortestPoint) ->
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
		{done } -> ok
	after 0 ->
		ThreadServer ! { getJob, self() },
		receive
			{ done } -> ok;
			{ job, Index, Amount, Scene, CanvasSize, Width, Height } ->
				io:format("traceWorker() PID ~w, Index ~w (~w %)~n", [self(), Index, trunc(Index/CanvasSize * 100)]),
				Upper = if
					Index+Amount >= CanvasSize -> CanvasSize-1;
					true -> Index+Amount-1
				end,
				Values = lists:map(
					fun(At) ->
						Pixel = #coords{x=-Width/2+At rem Width, y=-Height/2+At div Height, z=-300},
						cast(Scene, Pixel, #coords{})
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
	Main = self(),
	ThreadServer = spawn(fun() -> threadServer(0, ?PIXELS_AT_ONCE, Scene, Width*Height, Width, Height) end),
	Threads = lists:map(
		fun(_) -> spawn(fun() -> traceWorker(Main, ThreadServer) end) end,
		lists:seq(0, ?THREAD_COUNT-1)
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
			#sphere{radius=2000, coords = #coords{x=0,y=0,z=-12000}, light = true, color = #color{r=255,g=255,b=255}},
			#sphere{radius=2000, coords = #coords{x=0,y=4000,z=-12000}, light = false, color = #color{r=1,g=0,b=0}},
			#sphere{radius=2000, coords = #coords{x=4000,y=0,z=-12000}, light = false, color = #color{r=0,g=0,b=1}},
			#sphere{radius=2000, coords = #coords{x=4000,y=4000,z=-12000}, light = false, color = #color{r=0.5,g=0.5,b=0.5}},
			#sphere{radius=3000, coords = #coords{x=2000,y=2000,z=-20000}, light = false, color = #color{r=1,g=1,b=1}}
			%#sphere{radius=99999, coords = #coords{x=0,y=120000,z=0}, light = true},
			%#sphere{radius=99999, coords = #coords{x=-120000,y=0,z=0}, light = true},
			%#sphere{radius=99999, coords = #coords{x=120000,y=0,z=0}, light = true}
		],
		{X,X},
		1
	)
.
