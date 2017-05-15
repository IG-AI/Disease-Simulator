-module(movement).
-export([new_bounce_position/4, new_bounce_random_position/4, generate_position/1, generate_direction/0]).
-include("includes.hrl").

%%
%% @doc Generates a tuple containining a randomly generated x coordinate between 0 and X_max and 
%% a randomly generated y coordinate between 0 and Y_max.
%%
%% @param X_max The upper bound of the x-axis.
%% @param Y_max The upper bound of the Y-axis.
%%
%% @returns The tuple containing the x and y coordinates.
%%
-spec generate_position(bounds()) -> position().
generate_position({X_max,Y_max}) ->
    X = rand:uniform(X_max),
    Y = rand:uniform(Y_max),
    case collision_checker:check_collision(w_checker,X, Y) orelse collision_checker:check_collision(h_checker,X,Y) of
	true ->
	    generate_position({X_max, Y_max});
	false ->
	    {X,Y}        
    end.

%%
%% @doc Generate new x and y coordinates and a direction, based on an individuals current direction and x and y coordinates. 
%% The new coordinates will be one grid away from the original coordinates.
%%
%% @param X The current coordinate on the x-axis. 
%% @param Y The current coordinate on the y-axis. 
%% @param X_direction An integer between -1 and 1, representing the direction on the x-axis. 
%% @param Y_direction An integer between -1 and 1, representing the direction on the y-axis. 
%% @param X_max The upper bound of the x-axis. 
%% @param Y_max The upper bound of the y-axis. 
%%
%% @returns A tuple containing new x and y coordinates, and a new direction.
%%
-spec new_bounce_position(X :: integer(), Y :: integer(), {X_direction :: integer(),Y_direction :: integer()}, Bounds :: bounds()) -> {integer(),integer(), {integer(), integer()}}.
new_bounce_position(X, Y, {X_direction, Y_direction}, {X_max, Y_max}) ->
    X_wall_collision = collision_checker:check_collision(w_checker,X + X_direction, Y),
    Y_wall_collision = collision_checker:check_collision(w_checker,X, Y + Y_direction),

    case (X + X_direction >= X_max) orelse (X + X_direction =< 0) orelse (X_wall_collision) of
	true ->
	    New_X_direction = X_direction*(-1);
	_ ->
	    New_X_direction = X_direction
    end,
    case (Y + Y_direction >= Y_max) orelse (Y + Y_direction =< 0) orelse (Y_wall_collision) of 
	true ->
            New_Y_direction = Y_direction*(-1);
	_ ->
            New_Y_direction = Y_direction
    end,

    New_X = X + New_X_direction,
    New_Y = Y + New_Y_direction,
    {New_X, New_Y, {New_X_direction, New_Y_direction}}.

%%
%% @doc Generate new x and y coordinates and a randomized direction, based on an individuals current direction and x and y coordinates. 
%% The new coordinates will be one grid away from the original coordinates.  
%%
%% @param X The current coordinate on the x-axis. 
%% @param Y The current coordinate on the y-axis. 
%% @param X_direction An integer between -1 and 1, representing the direction on the x-axis. 
%% @param Y_direction An integer between -1 and 1, representing the direction on the y-axis. 
%% @param X_max The upper bound of the x-axis. 
%% @param Y_max The upper bound of the y-axis. 
%%
%% @returns A tuple containing new x and y coordinates, and a new direction.
%%
-spec new_bounce_random_position(X :: integer(), Y :: integer(), {X_direction :: integer(), Y_direction :: integer()}, Bounds :: bounds()) -> {integer(),integer(), integer()}.
new_bounce_random_position(X, Y, {X_direction, Y_direction}, {X_max, Y_max}) ->
    Wall_collision = collision_checker:check_collision(w_checker,X + X_direction, Y + Y_direction),

    case (X + X_direction >= X_max) orelse (X + X_direction =< 0) orelse (Y + Y_direction >= Y_max) orelse 
        (Y + Y_direction =< 0) orelse (Wall_collision) of
	true ->
            {New_X_direction, New_Y_direction} = new_direction(X, Y, generate_direction(), {X_max, Y_max});
	_ ->
	    New_X_direction = X_direction,
            New_Y_direction = Y_direction
    end,

    New_X = X + New_X_direction,
    New_Y = Y + New_Y_direction,

    {New_X, New_Y, {New_X_direction, New_Y_direction}}.

%%
%% @doc Generate a randomized direction, based on an individuals current direction and x and y coordinates. 
%%
%% @param X The current coordinate on the x-axis.
%% @param Y The current coordinate on the y-axis. 
%% @param X_direction An integer between -1 and 1, representing the direction on the x-axis. 
%% @param Y_direction An integer between -1 and 1, representing the direction on the y-axis. 
%% @param X_max The upper bound of the x-axis. 
%% @param Y_max The upper bound of the y-axis.
%%
%% @returns A tuple containing the new direction.
%%
%% @TODO Rename function to avoid confusion with generate_direction().
%%
-spec new_direction(X :: integer(), Y :: integer(), {X_direction :: integer(), Y_direction :: integer()}, Bounds :: bounds()) -> direction().
new_direction(X, Y, {X_direction, Y_direction}, {X_max, Y_max}) ->
    Wall_collision = collision_checker:check_collision(w_checker,X + X_direction, Y + Y_direction),

    case (X + X_direction >= X_max) orelse (X+ X_direction =< 0) orelse (Y + Y_direction >= Y_max) orelse 
        (Y + Y_direction =< 0) orelse (Wall_collision) of
	true ->
            {New_X_direction, New_Y_direction} = new_direction(X,Y, generate_direction(), {X_max, Y_max});
	_ ->
	    New_X_direction = X_direction,
            New_Y_direction = Y_direction
    end,
    {New_X_direction, New_Y_direction}.

%%
%% @doc Generate a randomized direction where both X and Y movement is not equal to 0.
%%
%% @return A new direction.
%% 
-spec generate_direction() -> direction().
generate_direction() ->
    Direction = {rand:uniform(3)-2,rand:uniform(3)-2},
    case Direction of
	{0, 0} ->
	    generate_direction();
	_ ->
	    Direction
    end.
