-module(wall_checker).
-include("includes.hrl").
-export([check_wall/1, get_wall_collision/2]).

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
	


		
		     
							   
	    
	    
