-module(a_star_demo).
-export([b1/0, dump_vertex/1]).

%% The representation of a vertex.
-type my_vertex() :: {integer(), integer()}.

-spec b1() -> ok.
b1() ->
  File = "map_one.adjmap",
  %% Heurestic underestimate function
  F = fun({X1, Y1}, {X2, Y2}) -> abs(X1 - X2) + abs(Y1 - Y2) end,
  G = graph:import(File, fun parse_vertex1/1),
  {Cost, Path} = a_star:run(G, {1,1}, {2,2}, F),
  io:format("Cost: ~p~nPath: ~p~n", [Cost, Path]).

%% Parses the string that holds a vertex.
-spec parse_vertex(string()) -> my_vertex().
parse_vertex([$(, X, $,, Y, $)]) -> {X - $0, Y - $0}.

%% Dumps a vertex to a string.
-spec dump_vertex(my_vertex()) -> string().
dump_vertex({X, Y}) -> [$(, X + $0, $,, Y + $0, $)].

parse_string([$( | S],X,Y,P) ->
    parse_string(S,X,Y,P);
parse_string([$,| S],X,Y,_) ->
    parse_string(S,X,Y,1);
parse_string([$) | _],X,Y,_) ->
    {X,Y};
parse_string([N | S],X,Y,0) ->
    parse_string(S,X+(N-$0),Y,0);
parse_string([N | S],X,Y,1) ->
    parse_string(S,X,Y+(N-$0),1).

parse_vertex1(S) ->
    parse_string(S,0,0,0).
