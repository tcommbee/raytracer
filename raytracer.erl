% Copyright 2009 René Kijewski, Jonas Dohse, David Knoetel, Stefan Friesel
%
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
% 
% You should have received a copy of the GNU Lesser General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(raytracer).
-export([trace/3, test/1]).

-record(color, {r = 0, g = 0, b = 0}).
-record(coords, {x = 0, y = 0, z = 0}).
-record(sphere, {radius = 1, coords = #coords{}, light = false, color = #color{}}).

-define(PIXELS_AT_ONCE, trunc(math:pow(2,12))).
-define(THREAD_COUNT, 5).

%debug prints (formatted) Text to stderr
debug(Text) ->
	Stderr = case get('PORT_STDERR') of
		undefined -> R = open_port({fd,0,2}, [out]), put('PORT_STDERR', R), R;
		R -> R
	end,
	port_command(Stderr, Text)
.
debug(Format, Parameters) -> debug( io_lib:format(Format, Parameters) ) .

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
		{ done } ->
			debug("<  Worker PID ~w, Index ~w (done)~n", [self(), CanvasSize]),
			ok
	after 0 ->
		ThreadServer ! { getJob, self() },
		receive
			{ done } ->
				debug("<  Worker PID ~w, Index ~w (done)~n", [self(), CanvasSize]),
				ok;
			{ job, Index, Amount } ->
				debug("<  Worker PID ~w, Index ~w (~w %)~n", [self(), Index, trunc(Index/CanvasSize * 100)]),
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
	end
.

spawnTraceWorker({Main, ThreadServer, Scene, CanvasSize, Width, Height}) ->
	spawn_link(fun() -> traceWorker(Main, ThreadServer, Scene, CanvasSize, Width, Height) end)
.

%setThreadJob replaces the to C assigned job with I
%  List: initially []
%  TS: List of {PID,Job}
%  C: PID in question
%  I: new job
%  returns: false if C is not in TS, otherwise new list
setThreadJob(_, [], _, _) -> false;
setThreadJob(List, [{T,_}|TS], T, I) -> [ {T,I} | List ++ TS ];
setThreadJob(List, [T|TS], C, I) -> setThreadJob([T|List], TS, C, I) .

%removeThreadJob removes a thread from a thread list
%  List: initially []
%  TS: List of {PID,Job}
%  C: PID to delete
%  returns: false if C was not in TS, otherwise new list
removeThreadJob(_, [], _) -> false;
removeThreadJob(List, [{T,_}|TS], T) -> List ++ TS;
removeThreadJob(List, [T|TS], C) -> removeThreadJob([T|List], TS, C) .

%replaceThreadJob replaces an old PID with a new PID
%  List: initially []
%  TS: List if {PID,Job}
%  Old: old PID
%  New: new PID
%  returns: { assigned job, new list }
%  throws: { thread_to_replace_is_unknown, [Old, New] } if Old is not in TS
replaceThreadJob(_, [], Old, New) -> throw({thread_to_replace_is_unknown, [Old, New]});
replaceThreadJob(List, [{Old,Index}|TS], Old, New) -> {Index, List ++ [{New,Index}|TS]};
replaceThreadJob(List, [T|TS], Old, New) -> replaceThreadJob([T|List], TS, Old, New) .

%isKnownThreadJob tests TS if it contains C
%  returns: boolean
isKnownThreadJob([], _) -> false;
isKnownThreadJob([{T,_}|_], T) -> true;
isKnownThreadJob([_|TS], C) -> isKnownThreadJob(TS, C) .

%threadServer anwers traceWorker's request for pixels to process
%  Index: current index (begin with 0)
%  CanvasSize: height * width
%  Jobs: List of { PID, Index/none }
%  RestartParams: Parameter for spawnTraceWorker
threadServer(Index, CanvasSize, [], _) when Index >= CanvasSize ->
	debug(" > Server all done!~n", []),
	ok;
threadServer(_, _, [], _) -> throw(there_are_no_threads_to_do_jobs);
threadServer(Index, CanvasSize, Jobs, _) when Index >= CanvasSize ->
	Caller = receive
		{'EXIT', Pid, Why} ->
			debug(" > Server PID ~w sent exit <~w>~n", [Pid, Why]),
			Pid;
		{getJob, Pid} ->
			debug(" > Server PID ~w will receive done~n", [Pid]),
			Pid!{done}, % send him {done} in either case
			Pid
	end,
	NJobs = case removeThreadJob([], Jobs, Caller) of 
		false -> Jobs;
		New -> New
	end,
	threadServer(Index, CanvasSize, NJobs, {})
;
threadServer(Index, CanvasSize, Jobs, RestartParams) ->
	receive
		{'EXIT', Caller, Why} ->
			debug(" > Server PID ~w killed <~w>", [Caller, Why]),
			{NJ, NI} = case isKnownThreadJob(Jobs, Caller) of
				false ->
					debug(" > Process unknown (?)~n", []),
					{Jobs, Index};
				true ->
					debug(" > Restarting ...~n", []),
					Pid = spawnTraceWorker(RestartParams),
					{ NJobs, IndexToSend, NIndex }  = case replaceThreadJob([], Jobs, Caller, Pid) of
						{none, New}     -> { New, Index, Index + ?PIXELS_AT_ONCE };
						{OldIndex, New} -> { New, OldIndex, Index }
					end,
					receive
						{'EXIT', Pid, Why} -> throw({thread_keeps_on_EXITing, Caller, Why});
						{getJob, Pid}      -> Pid ! { job, IndexToSend, ?PIXELS_AT_ONCE }
					end,
					{ NJobs, NIndex }
			end,
			threadServer(NI, CanvasSize, NJ, RestartParams);
		{getJob, Caller} ->
			case setThreadJob([], Jobs, Caller, Index) of
				false ->
					% TODO: what to do, if Caller is unknown?
					threadServer(Index, CanvasSize, Jobs, RestartParams);
				NJobs ->
					Caller ! { job, Index, ?PIXELS_AT_ONCE },
					threadServer(Index+?PIXELS_AT_ONCE, CanvasSize, NJobs, RestartParams)
			end
	end
.

startThreadServer(CanvasSize, Main, Scene, Width, Height) ->
	process_flag(trap_exit, true),
	RestartParams = { Main, self(), Scene, CanvasSize, Width, Height },
	Jobs = lists:map(
		fun(_) -> { spawnTraceWorker(RestartParams), none } end,
		lists:seq(0, ?THREAD_COUNT-1)
	),
	threadServer(0, CanvasSize, Jobs, RestartParams)
.

%collect collects traceWorker's results
%  Index: current index (begin with 0)
%  CanvasSize: height * width
%  Port: to write to
%  returns: nothing
collect(Index, CanvasSize, _, _) when Index >= CanvasSize -> ok;
collect(Index, CanvasSize, Port, ThreadServer) ->
	receive
		{ 'EXIT', ThreadServer, normal } ->
			collect(Index, CanvasSize, Port, ThreadServer);
		{ 'EXIT', ThreadServer, Why } ->
			debug(" > FATAL: The threadserver was killed~nWhy: ~w~n", [Why]),
			throw({thread_server_was_killed, Why});
		{ pixels, Index, Amount, Values } ->
			Data = lists:append(lists:map(fun(#color{r=R, g=G, b=B}) -> [trunc(R),trunc(G),trunc(B)] end, Values)),
			port_command(Port, Data),
			collect(Index + Amount, CanvasSize, Port, ThreadServer)
	end.

%produce a raytraced image
%  Port: port() to write to
%  Scene: a list of geometry and lightsources to be rendered
%  Dimensions: of the output image in px
%  returns: nothing
trace(Port, Scene, {Width, Height}) ->
	process_flag(trap_exit, true),
	Main = self(),
	CanvasSize = Width*Height,
	ThreadServer = spawn_link(fun() -> startThreadServer(CanvasSize, Main, Scene, Width, Height) end),
	port_command(Port, "P6\n" ++ integer_to_list(Width) ++ " " ++ integer_to_list(Height) ++"\n255\n"),
	collect(0, CanvasSize, Port, ThreadServer)
.

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
colorAdd(#color{r=R1,g=G1,b=B1}, #color{r=R2,g=G2,b=B2}) ->
	#color{r=R1+R2, g=G1+G2, b=B1+B2}.

intersections([], _, _) -> [];
intersections([First|RestOfWorld], StartPos, Target) -> [intersect(First, StartPos, Target) | intersections(RestOfWorld, StartPos, Target)].

intersect(Object, StartPos, Target) ->
	CS = vectorSub(Object#sphere.coords, StartPos),
	ST = vectorSub(StartPos, Target),
	R2 = Object#sphere.radius * Object#sphere.radius,

	STm2 = 1.0/vectorSqr(ST),
	CS2 = vectorSqr(CS),
	CStST = vectorMul(CS, ST),

	Left = - (CStST * STm2),
	Right = (Left*Left) + ((R2-CS2)*STm2),

	if
		Right < 0 -> {undefined, undefined, undefined, undefined};
		true ->
			L = Left - math:sqrt(Right),
			{Object, vectorAdd(scalarMul(ST, -L), StartPos), L, scalarMul(ST, -Left)}
	end.

testScene() -> [
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

].

test(X) ->
	Stdout = open_port({fd,0,1}, [out]),
	Result = trace(Stdout, testScene(), {X,X}),
	port_close(Stdout),
	Result
.
