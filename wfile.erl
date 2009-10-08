
-module(wfile).
-export([traceToFile/4]).

-import(lists).
-import(fil).

trace(Scene, _, _) ->
        Scene.

traceToFile(File, Scene, {Width, Height}, Passes) ->
        Picture = trace(Scene, {Width, Height}, Passes),
        Out = prep([Width, Height, 255] ++ Picture),
        case file:open(File, [write]) of
                {ok, Fd} ->
                        file:write(Fd, ["P2\n", "# Erlang Raytracer Output\n"] ++ Out),
                        file:close(Fd);
                {error, R} ->
                        exit(R)
        end.

prep(List) ->
        map(fun(E) -> integer_to_list(E) ++ ["\n"] end, List).
