-module(a_star_demo).
-export([b1/0, dump_vertex/1]).

%% The representation of a vertex.
-type my_vertex() :: {integer(), integer()}.

-spec b1() -> ok.
b1() ->
  File = "map_one.adjmap",
%    File = "output.txt",
  %% Heurestic underestimate function
  F = fun({X1, Y1}, {X2, Y2}) -> abs(X1 - X2) + abs(Y1 - Y2) end,
  G = graph:import(File, fun parse1/1),
  {Cost, Path} = a_star:run(G, {1,1}, {4,4}, F),
  io:format("Cost: ~p~nPath: ~p~n", [Cost, Path]),
  {Cost2, Path2} = a_star:run(G, {2,1}, {290,290}, F),
  io:format("Cost2: ~p~nPath2: ~p~n", [Cost2, Path2]),
  ok.

%% Parses the string that holds a vertex.
-spec parse_vertex(string()) -> my_vertex().
parse_vertex([$(, X, $,, Y, $)]) -> {X - $0, Y - $0}.

%% Dumps a vertex to a string.
-spec dump_vertex(my_vertex()) -> string().
dump_vertex({X, Y}) -> [$(, X + $0, $,, Y + $0, $)].

parse1(S) ->
    {_, [_,{S1, L1}, {S2, L2}]} = re:run(S, "([0-9]+)[^0-9]*([0-9]+)"),
    {A,_} = string:to_integer(string:substr(S, S1+1, L1)),
    {B,_} = string:to_integer(string:substr(S, S2+1, L2)),
    {A,B}.
