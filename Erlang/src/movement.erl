-module(movement).
-export([new_bounce_position/4, new_bounce_random_position/4, generate_position/1, generate_direction/0]).
-include("includes.hrl").

%%
%% @doc Generates a ttuple containining a randomly generated x coordinate between 0 and X_max and 
%% a randomly generated y coordinate between 0 and Y_max
%%
%% @param X_max the upper bound of the x-axis
%% @param Y_max the upper bound of the Y-axis
%%
%% @returns the tuple containing the x and y coordinates
%%
-spec generate_position(bounds()) -> position().
generate_position({X_max,Y_max}) ->
    X = rand:uniform(X_max),
    Y = rand:uniform(Y_max),
    case collision_checker:get_wall_collision(X, Y) of
	true ->
	    generate_position({X_max, Y_max});
	false ->
	    {X,Y}        
    end.

%%
%% @doc Generate a direction where both X and Y movement is not equal to 0
%%
%% @return A new direction
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

%%
%% @doc Generate new x and y coordinates of a process based on its current x and y coordinates and a Direction. 
%% The new coordinates will be one grid away from the original coordinates or the same as the original coordinates.  
%%
%% @param X the current coordinate on the x-axis 
%% @param Y the current coordinate on the y-axis 
%% @param Direction an integer between 1 and 9 
%%
%% @returns a tuple containing new x and y coordinates, and the direction the person is moving.
%%
-spec new_bounce_position(X :: integer(), Y :: integer(), {X_direction :: integer(),Y_direction :: integer()}, Bounds :: bounds()) -> {integer(),integer(), {integer(), integer()}}.
new_bounce_position(X, Y, {X_direction, Y_direction}, {X_max, Y_max}) ->
    X_wall_collision = collision_checker:get_wall_collision(X+X_direction, Y),
    Y_wall_collision = collision_checker:get_wall_collision(X, Y + Y_direction),

    case (X+X_direction >= X_max) orelse (X+X_direction =< 0) orelse (X_wall_collision) of
	true ->
	    New_X_direction = X_direction*(-1);
	_ ->
	    New_X_direction = X_direction
    end,
    case (Y+Y_direction >= Y_max) orelse (Y + Y_direction =< 0) orelse (Y_wall_collision) of 
	true ->
            New_Y_direction = Y_direction*(-1);
	_ ->
            New_Y_direction = Y_direction
    end,

		New_X = X + New_X_direction,
		New_Y = Y + New_Y_direction,
    {New_X, New_Y, {New_X_direction, New_Y_direction}}.


-spec new_direction(X :: integer(), Y :: integer(), {X_direction :: integer(), Y_direction :: integer()}, Bounds :: bounds()) -> direction().
new_direction(X, Y, {X_direction, Y_direction}, {X_max, Y_max}) ->
	Wall_collision = collision_checker:get_wall_collision(X+X_direction, Y+Y_direction),

    case (X+X_direction >= X_max) orelse (X+X_direction =< 0) orelse (Y+Y_direction >= Y_max) orelse 
        (Y + Y_direction =< 0) orelse (Wall_collision) of
	true ->
            {New_X_direction, New_Y_direction} = new_direction(X,Y, people:generate_direction(), {X_max,Y_max});
	_ ->
	    New_X_direction = X_direction,
            New_Y_direction = Y_direction
    end,

    {New_X_direction, New_Y_direction}.


-spec new_bounce_random_position(X :: integer(), Y :: integer(), {X_direction :: integer(), Y_direction :: integer()}, Bounds :: bounds()) -> {integer(),integer(), integer()}.
new_bounce_random_position(X, Y, {X_direction, Y_direction}, {X_max, Y_max}) ->
    Wall_collision = collision_checker:get_wall_collision(X+X_direction, Y+Y_direction),

    case (X+X_direction >= X_max) orelse (X+X_direction =< 0) orelse (Y+Y_direction >= Y_max) orelse (Y + Y_direction =< 0) orelse (Wall_collision) of
	true ->
            {New_X_direction, New_Y_direction} = new_direction(X,Y, people:generate_direction(), {X_max,Y_max});
	_ ->
	    New_X_direction = X_direction,
            New_Y_direction = Y_direction
    end,

    New_X = X + New_X_direction,
    New_Y = Y + New_Y_direction,

    {New_X, New_Y, {New_X_direction, New_Y_direction}}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Testing generate_start_positions when no positions are generated.
%%
%% genarate_start_positions_zero_test() ->
%%     Bounds = {10,10},
%%     Number_of_positions = 0,
%%     Start_1 = [],
%%     Result_1 = generate_start_positions(Number_of_positions, Bounds, Start_1),
%%     ?assertEqual(Start_1, Result_1),
%%     Start_2 = [{1,1},{2,2},{3,3}],
%%     Result_2 = generate_start_positions(Number_of_positions, Bounds, Start_2),
%%     ?assertEqual(Start_2, Result_2).

%% %%
%% %% Testing generate_start_positions when one position is generated.
%% %%
%% genarate_start_positions_one_test() ->
%%     {X_max, Y_max} = {10,10},
%%     Number_of_positions = 1,
%%     Start = [],
%%     Result = generate_start_positions(Number_of_positions, {X_max,Y_max}, Start),
%%     [{X,Y}] = Result,
%%     ?assertEqual(length(Result), Number_of_positions),
%%     ?assert(X =< X_max),
%%     ?assert(X >= 0),
%%     ?assert(Y =< Y_max),
%%     ?assert(Y >= 0).

%% %%
%% %% Testing generate_start_positions when several (10) positions are generated.
%% %%
%% genarate_start_positions_several_test() ->
%%     {X_max, Y_max} = {10,10},
%%     Number_of_positions = 10,
%%     Start = [],
%%     Result = generate_start_positions(Number_of_positions, {X_max,Y_max}, Start),
%%     ?assertEqual(length(Result), Number_of_positions).

%% %%
%% %% Testing new_position by testing all outcomes.
%% %%
%% %% new_position_test() ->
%% %%      [?assertEqual({9,11},new_position(10,10,1)),
%% %%       ?assertEqual({10,11},new_position(10,10,2)),
%% %%       ?assertEqual({11,11},new_position(10,10,3)),
%% %%       ?assertEqual({9,10},new_position(10,10,4)),
%% %%       ?assertEqual({10,10},new_position(10,10,5)),
%% %%       ?assertEqual({11,10},new_position(10,10,6)),
%% %%       ?assertEqual({9,9},new_position(10,10,7)),
%% %%       ?assertEqual({10,9},new_position(10,10,8)),
%% %%       ?assertEqual({11,9},new_position(10,10,9))].

%% %%
%% %% Testing valid_possition when X or Y is within bounds.
%% %%
%% validate_position_within_bounds_test() ->
%%     Bounds = {10, 10},
%%     {X_start, Y_start} = {5, 5},
%%     {X_valid, Y_valid} = {7, 7},

%%     Position = validate_position(X_start, Y_start, X_valid, Y_valid, Bounds),
%%     ?assertEqual(Position, {X_valid, Y_valid}).

%% %%
%% %% Testing valid_possition when X or Y is out of bounds.
%% %%
%% validate_position_out_of_bounds_test() ->
%%     Bounds = {10, 10},
%%     {X_start, Y_start} = {5, 5},
%%     {X_valid, Y_valid} = {7, 7},

%%     % Case 1: when X is larger than the upper bound
%%     X_over = 11,
%%     Case_1 = validate_position(X_start, Y_start, X_over, Y_valid, Bounds),
%%     ?assertEqual(Case_1, {X_start, Y_start}),

%%     % Case 2: when X is smaller than the lower bound
%%     X_under = -1,
%%     Case_2 = validate_position(X_start, Y_start, X_under, Y_valid, Bounds),
%%     ?assertEqual(Case_2, {X_start, Y_start}),

%%     % Case 3: when Y is larger than the upper bound
%%     Y_over = 11,
%%     Case_3 = validate_position(X_start, Y_start, X_valid, Y_over, Bounds),
%%     ?assertEqual(Case_3, {X_start, Y_start}),

%%     % Case 4: when Y is smaller than the lower bound
%%     Y_under = -1,
%%     Case_4 = validate_position(X_start, Y_start, X_valid, Y_under, Bounds),
%%     ?assertEqual(Case_4, {X_start, Y_start}).

%% %%
%% %% Testing the edge cases (X or Y on Bounds) of valid_position.
%% %%
%% validate_position_edge_cases_test() ->
%%     Bounds = {10, 10},
%%     {X_start, Y_start} = {5, 5},
%%     {X_valid, Y_valid} = {7, 7},

%%     % Case 1: when X is on than the upper bound
%%     X_on_upper = 10,
%%     Case_1 = validate_position(X_start, Y_start, X_on_upper, Y_valid, Bounds),
%%     ?assertEqual(Case_1, {X_on_upper, Y_valid}),

%%     % Case 2: when X is on than the lower bound
%%     X_on_lower = 0,
%%     Case_2 = validate_position(X_start, Y_start, X_on_lower, Y_valid, Bounds),
%%     ?assertEqual(Case_2, {X_on_lower, Y_valid}),

%%     % Case 3: when Y is on than the upper bound
%%     Y_on_upper = 10,
%%     Case_3 = validate_position(X_start, Y_start, X_valid, Y_on_upper, Bounds),
%%     ?assertEqual(Case_3, {X_valid, Y_on_upper}),

%%     % Case 4: when Y is on than the lower bound
%%     Y_on_lower = 0,
%%     Case_4 = validate_position(X_start, Y_start, X_valid, Y_on_lower, Bounds),
%%     ?assertEqual(Case_4, {X_valid, Y_on_lower}).
