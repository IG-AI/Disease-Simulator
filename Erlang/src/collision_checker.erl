-module(collision_checker).
-include("includes.hrl").
-export([check_hospital/1, check_wall/1, get_wall_collision/2, get_hospital_location/2]).

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
%% @doc This function is spawned in a process and is receive a position
%% to check if it's a hospital or not, then responds to the sending process with the answer.
%%
%% @param A map with hospitals.
%% 
-spec check_hospital(Hospital :: map()) -> no_return().
check_hospital(Hospital)->
    receive
	{PID, X, Y} when is_integer(X) and is_integer(Y) ->
	    case maps:find(X, Hospital) of
		{ok, YW} ->
		    PID ! {hospital, lists:member(Y, YW)};
		_ ->
		    PID ! {hospital, false}
	    end
    end,
    check_hospital(Hospital).

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
	
%%
%% @doc Checks if a position is on a hospital tile
%%
%% @param X The X-coordinate to check
%% @param Y The Y-coordinate to check
%% @return a boolean showing if the given position is on a hospital tile. True if it is, else false.
%% 
-spec get_hospital_location(X :: integer(), Y :: integer()) -> boolean().
get_hospital_location(X, Y) ->
    h_checker ! {self(), X, Y},
    receive 
	{hospital, true} ->
	    true;
	{hospital, _} ->
	    false
    end.
	



		
		     
							   
	    
	    
