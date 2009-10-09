-module(raytracer).
-export([trace/3, test/1,intersect/3]).

-record(color, {r = 0, g = 0, b = 0}).
-record(coords, {x = 0, y = 0, z = 0}).
-record(sphere, {radius = 1, coords = #coords{}, light = false, color = #color{}}).

-define(PIXELS_AT_ONCE, trunc(math:pow(2,12))).
-define(THREAD_COUNT, 5).

%cast a ray
%  World: a list of geometry and lightsources the ray is cast into
%  StartPos: current "position" of ray
%  Target: point the ray is directed at
%  Depth: recursion depth, shall be 0 in first step
%  returns: lightness as color
cast(World, StartPos, Target, Depth) ->
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
		not is_record(Impact, coords), Depth > 0 ->
			Dir = vectorSub(Target, StartPos),
			Z = vectorMul(Dir, #coords{x=0.707106781,y=-0.707106781,z=0})/vectorAbs(Dir),
			if
				Z < 0 -> #color{r=0,g=0,b=0};
				true -> colorScale(#color{r = 30, g = 30, b = 30}, Z*Z)
			end;
		not is_record(Impact, coords)-> #color{r = 67, g = 111, b = 140};
		Obstacle#sphere.light == false ->
			Glow = colorScale(Obstacle#sphere.color, 0.1*255),
			LightColor = colorScale(cast(World, Impact, reflect(Obstacle, StartPos, Impact), Depth+1), 0.9),
			Combined = colorAdd(Glow, LightColor),
			C = Obstacle#sphere.color, colorMul(C, Combined);
		Obstacle#sphere.light == true -> LightSource = Obstacle, lightness(LightSource, StartPos, Impact, ShortestPoint)
	end.

cast(World, Target) ->
	cast(World, #coords{x=0, y=0, z=0}, Target, 0).

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

%traceWorker rendered a range of pixels ("worker thread")
%  Main: PID of process to receive the rendered pixels
%  ThreadServer: PID of process to ask for pixels to render
traceWorker(Main, ThreadServer, Scene, CanvasSize, Width, Height) ->
	receive
		{done } -> ok
	after 0 ->
		ThreadServer ! { getJob, self() },
		receive
			{ done } -> ok;
			{ job, Index, Amount } ->
				io:format("traceWorker() PID ~w, Index ~w (~w %)~n", [self(), Index, trunc(Index/CanvasSize * 100)]),
				Upper = if
					Index+Amount >= CanvasSize -> CanvasSize-1;
					true -> Index+Amount-1
				end,
				Z = -300*Width/420,
				Values = lists:map(
					fun(At) ->
						Pixel = #coords{x=-Width/2+At rem Width, y=-Height/2+At div Height, z=Z},
						cast(Scene, Pixel)
					end,
					lists:seq(Index, Upper)
				),
				Main ! { pixels, Index, Amount, Values },
				traceWorker(Main, ThreadServer, Scene, CanvasSize, Width, Height)
		end
	end.

setThreadJob(_, [], _, _) -> false;
setThreadJob(List, [{T,_}|TS], T, I) -> {true, [ {T,I} | List ++ TS ]};
setThreadJob(List, [T|TS], C, I) -> setThreadJob([T|List], TS, C, I) .

removeThreadJob(_, [], _) -> false;
removeThreadJob(List, [{T,_}|TS], T) -> {true, List ++ TS};
removeThreadJob(List, [T|TS], C) -> removeThreadJob([T|List], TS, C).

%threadServer anwers @traceWorker/2's request for pixels to process
%  Index: current index (begin with 0)
%  CanvasSize: height * width
%  Jobs: List of { PID, Index/none }
threadServer(_, _, []) -> ok;
threadServer(Index, CanvasSize, Jobs) when Index >= CanvasSize ->
	receive
		{getJob, Caller} ->
			Caller ! { done }, % send him {done} in either case
			case removeThreadJob([], Jobs, Caller) of
				{ true, NJobs } ->
					threadServer(Index, CanvasSize, NJobs);
				_ ->
					% TODO: what to do, if Caller is unknown?
					threadServer(Index, CanvasSize, Jobs)
			end
	end
;
threadServer(Index, CanvasSize, Jobs) ->
	receive
		{getJob, Caller} ->
			case setThreadJob([], Jobs, Caller, Index) of
				{ true, NJobs } ->
					Caller ! { job, Index, ?PIXELS_AT_ONCE },
					threadServer(Index+?PIXELS_AT_ONCE, CanvasSize, NJobs);
				_ ->
					% TODO: what to do, if Caller is unknown?
					threadServer(Index, CanvasSize, Jobs)
			end
	end
.

startThreadServer(CanvasSize, Main, Scene, Width, Height) ->
	ThreadServer = self(),
	Jobs = lists:map(
		fun(_) -> {
			spawn_link(fun() ->
				traceWorker(Main, ThreadServer, Scene, CanvasSize, Width, Height)
			end),
			none
		} end,
		lists:seq(0, ?THREAD_COUNT-1)
	),
	threadServer(0, CanvasSize, Jobs)
.

%collect collects @traceWorker/2's results
%  Index: current index (begin with 0)
%  CanvasSize: height * width
%  Fd: file handle for output
collect(Index, CanvasSize, _) when Index >= CanvasSize -> ok;
collect(Index, CanvasSize, Fd) ->
	receive
		{ pixels, Index, Amount, Values } ->
			file:write(Fd, prep(lists:append(lists:map(fun colorToList/1, Values)))),
			collect(Index + Amount, CanvasSize, Fd)
	end.

%produce a raytraced image
%  File: filename for output
%  Scene: a list of geometry and lightsources to be rendered
%  Dimensions: of the output image in px
%  returns: nothing
trace(File, Scene, {Width, Height}) ->
	Main = self(),
	CanvasSize = Width*Height,
	spawn(fun() -> startThreadServer(CanvasSize, Main, Scene, Width, Height) end),
	case file:open(File, [write]) of
		{ok, Fd} ->
			file:write(Fd, ["P3\n", "# Erlang Raytracer Output"] ++ prep([Width, Height, 255])),
			collect(0, CanvasSize , Fd),
			file:close(Fd);
		{error, R} ->
			exit(R)
	end,
	ok
.

prep(List) ->
	lists:map(fun(R) -> [$\n|integer_to_list(trunc(R))] end, List).

vectorSqr(#coords{x=X, y=Y, z=Z}) -> (X*X + Y*Y + Z*Z).
vectorMul(#coords{x=X, y=Y, z=Z}, #coords{x=A, y=B, z=C}) -> A*X + B*Y + C*Z .
scalarMul(#coords{x=X, y=Y, z=Z}, L) -> #coords{x=L*X, y=L*Y, z=L*Z} .
vectorAbs(V) -> math:sqrt( vectorSqr(V) ) .
vectorAdd(#coords{x=X, y=Y, z=Z}, #coords{x=A, y=B, z=C}) -> #coords{x=A+X, y=B+Y, z=C+Z} .
vectorSub(#coords{x=X, y=Y, z=Z}, #coords{x=A, y=B, z=C}) -> #coords{x=X-A, y=Y-B, z=Z-C} .

colorToList(#color{r=R, g=G, b=B}) -> [R,G,B].
colorMul(#color{r=R1,g=G1,b=B1}, #color{r=R2,g=G2,b=B2}) ->
	#color{r=R1*R2, g=G1*G2, b=B1*B2}.
colorScale(#color{r=R,g=G,b=B}, S) ->
	#color{r=R*S, g=G*S, b=B*S}.
colorAdd(#color{r=R1,g=G1,b=B1}, #color{r=R2,g=G2,b=B2}) ->
	#color{r=R1+R2, g=G1+G2, b=B1+B2}.

intersections([], _, _) -> [];
intersections([First|RestOfWorld], StartPos, Target) -> [intersect(First, StartPos, Target) | intersections(RestOfWorld, StartPos, Target)].

intersect(Object, StartPos, Target) ->
	CS = vectorSub(Object#sphere.coords, StartPos),
	ST = vectorSub(StartPos, Target),
	R2 = Object#sphere.radius * Object#sphere.radius,

	ST2 = vectorSqr(ST),
	CS2 = vectorSqr(CS),
	CStST = vectorMul(CS, ST),

	Left = - (CStST / ST2),
	Right = (Left*Left) + ((R2-CS2)/ST2),

	if
		Right < 0 -> {undefined, undefined, undefined, undefined};
		true ->
			L = Left - math:sqrt(Right),
			{Object, vectorAdd(scalarMul(ST, -L), StartPos), L, scalarMul(ST, -Left)}
	end.

test(X) ->
	trace("X.pnm",
		[
			#sphere{radius=3000, coords = #coords{x=0,y=0,z=-8000}, light = false, color = #color{r=0.72,g=0.6,b=0.2}},
			#sphere{radius=9997200, coords = #coords{x=0,y=10000000,z=-8000}, light = false, color = #color{r=0.5,g=0.5,b=0.5}},
			#sphere{radius=600, coords = #coords{x=-2000,y=-2200,z=-6000}, light = true, color = #color{r=255,g=255,b=255}},
			#sphere{radius=600, coords = #coords{x=0,y=-2200,z=-5000}, light = false, color = #color{r=1,g=1,b=1}},
			#sphere{radius=600, coords = #coords{x=2000,y=-2200,z=-4000}, light = true, color = #color{r=255,g=0,b=0}},
			#sphere{radius=600, coords = #coords{x=-2000,y=0,z=-5000}, light = true, color = #color{r=100,g=0,b=255}},
			#sphere{radius=600, coords = #coords{x=2000,y=-0,z=-5000}, light = true, color = #color{r=255,g=255,b=0}},
			#sphere{radius=600, coords = #coords{x=-2000,y=2200,z=-6000}, light = true, color = #color{r=128,g=255,b=0}},
			#sphere{radius=600, coords = #coords{x=0,y=2200,z=-5000}, light = false, color = #color{r=1,g=1,b=1}},
			#sphere{radius=600, coords = #coords{x=2000,y=2200,z=-4000}, light = true, color = #color{r=0,g=0,b=255}}
		],
		{X,X}
	).
