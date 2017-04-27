-module(adj_map).
-export([adj_map/0]).


adj_map() ->
    {X_max, Y_max, Walls, Hospital} = {6, 6, #{1=>[2, 3], 2=>[1, 2, 3], 3=>[2, 3, 4], 4=>[0, 3], 5=>[0]}, {}},
    register(checker, spawn(fun() -> check_wall(Walls) end)),
    G=row(X_max-5, Y_max-5, X_max-5, Y_max-5, [], [], []),
%    D = unregister(checker),
    io:format("G: ~p ~n",[G]).
	
    

%-spec row(X :: integer(), Y :: integer(), X_MAX :: integer(), X_POS :: [{integer(), integer()}], X_MOV :: []
row(_, -1, _, _, Pos, Mov, _) ->
    {Pos, Mov};


row(X, Y, X_max, Y_max, Pos, Mov, Prev) ->
    P = [{X_valid, Y} || X_valid  <- lists:seq(0, X_max), not get_wall_collision(X_valid, Y)], 
    io:format("P: ~p ~n",[P]),
    M = [{{X1,Y1},{X2,Y2}, 1} || {X1,Y1} <- P, {X2,Y2} <- Prev, ((X1 =:= X2) and (Y1 =:= Y2+1))],
    io:format("M: ~p ~n",[M]),
    K = [{{X1,Y1},{X2,Y2}, 1} || {X1,Y1} <- P, {X2,Y2} <- P, ((X1+1 =:= X2) and (Y1 =:= Y2))],
    io:format("K: ~p ~n",[K]),
    
    L = lists:merge(M,  K),
    io:format("L: ~p ~n",[L]),
    row(X, Y-1, X_max, Y_max ,[P | Pos], [L | Mov], P).
    





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
	


