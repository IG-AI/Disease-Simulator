-module(adj_map).
-export([adj_map/0]).
-include("includes.hrl").



-spec adj_map() -> adj_list().
adj_map() ->
    {X_max, Y_max, Walls, _Hospital} = {6, 6, #{1=>[2, 3], 2=>[1, 2, 3], 3=>[2, 3, 4], 4=>[0, 3], 5=>[0]}, {}},
    register(checker, spawn(fun() -> check_wall(Walls) end)),
    Mod = 1,
    {Pos, Mov} = row(X_max-Mod, Y_max-Mod, {X_max-Mod, Y_max-Mod}, [], [], []),
    unregister(checker),
    file:write_file("output.txt",  io_lib:fwrite("~p ~p undirected d~n~s ~n~s ~n",[length(Pos), length(Mov), pos_str(Pos,[]), pos_str(Mov, [])])).

-spec pos_str(pos_list() | adj_list(), [non_neg_integer()]) -> pos_list() | adj_list().        
pos_str([], Result)->
    Result;

pos_str([{X, Y} | T], Result) ->
    New_str = ["(" ++ integer_to_list(X) ++ "," ++ integer_to_list(Y) ++ ") "],
    pos_str(T, [Result | New_str]);
    
pos_str([{{X1, Y1},{X2, Y2}, Cost} | T], Result) ->
    New_str = ["(" ++ integer_to_list(X1) ++ "," ++ integer_to_list(Y1) ++ ") " ++ 
                   "(" ++ integer_to_list(X2) ++ "," ++ integer_to_list(Y2) ++ ") " ++ integer_to_list(Cost) ++ "\n"],
    pos_str(T, [Result | New_str]).
    

-spec row(X :: integer(), Y :: integer(), bounds(), Pos :: pos_list(), Mov :: adj_list(), Prev :: pos_list()) -> {pos_list(), adj_list()}.
row(_, -1, _, Pos, Mov, _) ->
    io:format("Pos: ~p ~n Mov: ~p ~n",[length(Pos),length(Mov)]),
    {Pos, Mov};


row(X, Y, {X_max, Y_max}, Pos, Mov, Prev) ->
    New_pos = [{X_valid, Y} || X_valid  <- lists:seq(0, X_max), not get_wall_collision(X_valid, Y)], 
    Down = [{{X1,Y1},{X2,Y2}, 1} || {X1,Y1} <- New_pos, {X2,Y2} <- Prev, ((X1 =:= X2) and (Y1+1 =:= Y2))],
    Left = [{{X1,Y1},{X2,Y2}, 1} || {X1,Y1} <- New_pos, {X2,Y2} <- New_pos, ((X1+1 =:= X2) and (Y1 =:= Y2))],
    row(X, Y-1, {X_max, Y_max},lists:merge(New_pos, Pos), Down++Left++Mov, New_pos).
    


%%
%% @doc This function is spawned in a process and is receive a position
%% to check if it's a wall or not, then responds to the sending process with the answer.
%%
%% @param A map with walls.
%% 
-spec check_wall(Walls :: map()) -> no_return().
check_wall(Walls)->
    receive
	{PID, X, Y} when is_integer(X) and is_integer(Y) ->
	    case maps:find(X, Walls) of
		{ok, YW} ->
		    PID ! {wall, lists:member(Y, YW)};
		_ ->
		    PID ! {wall, false}
	    end
    end,
    check_wall(Walls).

%%
%% @doc Checks if a position is inside a wall
%%
%% @param X The X-coordinate to check
%% @param Y The Y-coordinate to check
%% @return a boolean showing if the given position is a wall. True if it's a wall, else false.
%% 
-spec get_wall_collision(X :: integer(), Y :: integer()) -> boolean().
get_wall_collision(X, Y) ->
    checker ! {self(), X, Y},
    receive 
	{wall, true} ->
	    true;
	{wall, _} ->
	    false
    end.
	


