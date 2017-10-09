-module(collision_checker).

-include("includes.hrl").
-export([check_collision/3,check/1]).

%%
%% @doc This function is spawned in a process. It receives a position
%% and checks if the position is part of the provided map of obstacles, then sends the anwser to the calling process.
%%
%% @param A map with obstacles.
-spec check(Obstacles :: map()) -> no_return().
check(Obstacles)->
    receive
	{PID, X, Y} when is_integer(X) and is_integer(Y) ->
	    case maps:find(X, Obstacles) of
		{ok, Y_positions} ->
		    PID ! lists:member(Y, Y_positions);
		_ ->
		    PID ! false
	    end
    end,
    check(Obstacles).

%%
%% @doc Checks if a position is an obstacle.
%%
%% @param X The x coordinate to check.
%% @param Y The y coordinate to check.
%%
%% @returns true if the given position is an obstacle, false otherwise.
%%
-spec check_collision(Checker :: pid() | atom() ,X :: integer(), Y :: integer()) -> boolean().
check_collision(Checker,X, Y) ->
    Checker ! {self(), X, Y},
    receive
	 true ->
	    true;
	false ->
	    false
    end.

