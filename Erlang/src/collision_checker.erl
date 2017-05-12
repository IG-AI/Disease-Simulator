-module(collision_checker).

-include("includes.hrl").
-export([check_hospital/1, check_wall/1, get_wall_collision/2, get_hospital_location/2]).

%%
%% @doc This function is spawned in a process. It receives a position
%% and checks if the position is part of a wall or not, then  sends the anwser to the calling process.
%%
%% @param A map with walls.
%%
%% @returns Nothing
%%
-spec check_wall(Walls :: map()) -> no_return().
check_wall(Walls)->
    receive
	{PID, X, Y} when is_integer(X) and is_integer(Y) ->
	    case maps:find(X, Walls) of
		{ok, Y_positions} ->
		    PID ! {wall, lists:member(Y, Y_positions)};
		_ ->
		    PID ! {wall, false}
	    end
    end,
    check_wall(Walls).

%%
%% @doc This function is spawned in a process. It receives a position
%% and checks if the position is part of a hospital or not, then sends the anwser to the calling process.
%%
%% @param A map with hospitals.
%%
-spec check_hospital(Hospital :: map()) -> no_return().
check_hospital(Hospital)->
    receive
	{PID, X, Y} when is_integer(X) and is_integer(Y) ->
	    case maps:find(X, Hospital) of
		{ok, Y_positions} ->
		    PID ! {hospital, lists:member(Y, Y_positions)};
		_ ->
		    PID ! {hospital, false}
	    end
    end,
    check_hospital(Hospital).

%%
%% @doc Checks if a position is inside a wall.
%%
%% @param X The x coordinate to check.
%% @param Y The y coordinate to check.
%%
%% @returns true if the given position is on a wall grid, false otherwise.
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
%% @doc Checks if a position is part of a hospital. 
%%
%% @param X The x coordinate to check.
%% @param Y The y coordinate to check.
%%
%% @returns true if the given position is on a hospital grid, false otherwise.
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

