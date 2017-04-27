-module(movement).
-export([new_position/4, new_rand_position/3, validate_position/5, generate_start_positions/3]).
-include("includes.hrl").


%%
%% @doc Generates Amonunt number of tuples and append them to Result. Each tuple containins a randomly generated x coordinate between 0 and X_max and 
%% a randomly generated y coordinate between 0 and Y_max
%%
%% @param Amount the amount of positions to be generated
%% @param X_max the upper bound of the x-axis
%% @param Y_max the upper bound of the Y-axis
%% @param Result the list to which the new positions are to be appended	
%%
%% @returns Result with the new positions appended to it
%%
-spec generate_start_positions(Amount :: integer(), bounds(), [position()]) -> [position()].
generate_start_positions(0,_,Result) ->
    Result;

generate_start_positions(Amount, {X_max,Y_max}, Result) ->
    X = rand:uniform(X_max),
    Y = rand:uniform(Y_max),
    case wall_checker:get_wall_collision(X, Y) of
	true ->
	    generate_start_positions(Amount, {X_max, Y_max}, Result);
	false ->
	    generate_start_positions(Amount-1, {X_max, Y_max},[{X, Y} | Result])
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

-spec new_position(X :: integer(), Y :: integer(), {X_direction :: integer(),Y_direction :: integer()}, Bounds :: bounds()) -> {integer(),integer(), {integer(), integer()}}.
new_position(X, Y, {X_direction, Y_direction}, {X_max, Y_max}) ->
    Wall_collision = wall_checker:get_wall_collision(X+X_direction, Y+Y_direction),

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

-spec new_direction(X :: integer(), Y :: integer(), {X_direction :: integer(), Y_direction :: integer()}, Bounds :: bounds()) -> direction().
new_direction(X, Y, {X_direction, Y_direction}, {X_max, Y_max}) ->
	Wall_collision = wall_checker:get_wall_collision(X+X_direction, Y+Y_direction),

    case (X+X_direction >= X_max) orelse (X+X_direction =< 0) orelse (Y+Y_direction >= Y_max) orelse (Y + Y_direction =< 0) orelse (Wall_collision) of
	true ->
				{New_X_direction, New_Y_direction} = new_direction(X,Y, people:generate_direction(), {X_max,Y_max});
	_ ->
	    New_X_direction = X_direction,
			New_Y_direction = Y_direction
    end,

    {New_X_direction, New_Y_direction}.



    %% case Direction of 
    %%     1 ->
    %% 	    New_Direction = new_direction(X, Y, Direction, Bounds),
    %%         {X-1, Y+1, New_Direction};
    %%     2 ->
    %%         {X, Y+1};
    %%     3 ->
    %%         {X+1, Y+1};
    %%     4 ->
    %%         {X-1, Y};
    %%     5 ->
    %%         {X+1, Y-1};
    %%     6 ->
    %%         {X+1, Y};
    %%     7 ->
    %%         {X-1, Y-1};
    %%     8 ->
    %%         {X, Y-1};
    %%     9 ->
    %%         {X, Y};
    %%     end.

%%
%% @doc Randomly generates new x and y coordinates of a process based on its current x and y coordinates and the upper bounds of the x-axis and y-axis. 
%% The new coordinates will be one grid away from the original coordinates or the same as the original coordinates. 
%% All coordinates will be larger or equal to 0 and smaller or equal to the upper bound on both the x-axis and the y-axis
%%
%% @param X the current coordinate on the x-axis 
%% @param Y the current coordinate on the y-axis 
%% @param X_max the upper bound of the x-axis
%% @param Y_max the upper bound of the Y-axis
%%
%% @returns a tuple containing new x and y coordinates
%%
-spec new_rand_position(X :: integer(), Y :: integer(), bounds()) -> no_return().
new_rand_position(X, Y, {X_max, Y_max}) ->
    {X_new, Y_new} = new_position(X, Y, rand:uniform(9), Y),
    validate_position(X, Y, X_new, Y_new, {X_max, Y_max}).
    
%%
%% @doc Returns  {X_new, Y_new} if X-new is greater or equal than 0 and smaller or equal to the upper bounds, X_max and Y_max respectively, otherwise it returns {X_old, Y_old} 
%%
%% @param X_old the current coordinate on the x-axis 
%% @param Y_old the current coordinate on the y-axis 
%% @param X_new the coordinate on the x-axis that is to be validated 
%% @param Y_new the coordinate on the x-axis that is to be validated
%% @param X_max the upper bound of the x-axis
%% @param Y-max the upper bound of the Y-axis
%%
%% @returns a tuple with the validated coordinates
%%
-spec validate_position(X_old :: integer(), Y_old :: integer(), X_new :: integer(), X_new :: integer(), bounds()) -> {integer(),integer()}.
validate_position(X_old, Y_old, X_new, Y_new, {X_max, Y_max}) ->
    if
        X_new > X_max ->
            {X_old, Y_old};
        X_new < 0 ->
            {X_old, Y_old};
        Y_new > Y_max ->
            {X_old, Y_old};
        Y_new < 0 -> 
            {X_old,Y_old};
        true -> 
            {X_new, Y_new}
    end.

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
