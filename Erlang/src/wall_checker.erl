-module(wall_checker).
-include("includes.hrl").
-export([check_wall/1, get_wall_collision/2]).


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


get_wall_collision(X, Y) ->
    checker ! {self(), X, Y},
    receive 
	{wall, true} ->
	    true;
	{wall, _} ->
	    false
    end.
	


		
		     
							   
	    
	    
