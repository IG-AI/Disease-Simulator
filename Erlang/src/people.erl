-module(people).
-export([spawn_people/5, people/2]).

-include("includes.hrl").

%%
%% @doc Spawn the people that will be used in the simulation. 
%%
%% @param State The current state that you would like to add people to. Will be empty most of the time.
%% @param Amount The amount of people that you would like to spawn.
%% @param X_max the upper bound on the x-axis
%% @param Y_max the upper bound on the y-axis
%% @param S the status (healthy = 0 or infected = 1) for the next process to be spawned
%% @param Status a list with starting statuses for the people. Should be the same length as Amount.
%% @param X the x-coordinate for the next process to be spawned, must be smaller than X_max and larger than 0
%% @param Y the y-coordinate for the next process to be spawned, must be smaller than Y_max and larger than 0
%% @param Positions a list with starting positions for the people. Should be the same length as Amount.
%% 
%% @returns A state with Amount number of people with their status set to 0. 
%%
-spec spawn_people(State :: state(), Amount :: integer(), Bounds :: bounds(), [integer()], [position()]) -> state().
spawn_people(State, 0, _, _, _) ->
    State;

spawn_people(State, Amount, {X_max, Y_max}, [S | Status],[{X,Y} | Positions]) ->
    PID = spawn(fun() -> people({S, X, Y}, {X_max,Y_max}) end),
    spawn_people(State ++ [{PID, S,X,Y}], Amount-1, {X_max,Y_max}, Status, Positions).

%%
%% @doc Loop untill it receives the atom stop. The process will update X and Y with a new random position
%% and send a tagged tuple with its new position and its pid to the registred processes master if it receives the atom ready.  
%% 
%% @param S the new state of the person. Representing the health of the person.
%% @param X the new x coordinate of the person.
%% @param Y the new y coordinate of the person.
%% @param Bounds the limits of @see X and @see Y.
%%
%% @returns done
%%
-spec people(person(), Bounds :: bounds()) -> done.
people({S,X,Y}, Bounds) ->
    receive
        ready ->           
            {X_new, Y_new} = movement:new_rand_position(X,Y,Bounds),           
            master ! {work, {self(), S, X_new, Y_new}},            
            people({S, X_new, Y_new}, Bounds);  
        {infect_people, Probability, Targets} ->
            P = fun (Z) -> if
                               Z =< Probability -> true;
                               true -> false
                           end                           
                end,
            [PID ! get_infected || P(rand:uniform()) ,PID <- Targets],
            people({S, X, Y}, Bounds);

        get_infected ->
            people({1, X, Y}, Bounds);

        stop ->
            done
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% %% 
%% %% Testing spawn_people when no processes are spawned. 
%% %%
%% spawn_people_none_test() ->
%%     Bounds = {10,10},
%%     State_start =[],
%%     Start_position = [{5,5}],
%%     State = spawn_people(State_start, 0, Bounds, Start_position),
%%     ?assertEqual(State_start, State).

%% %%
%% %% Testing spawn_people when a single process is spawned. 
%% %%
%% spawn_people_single_test() ->
%%     {X_max, Y_max} = {10,10},
%%     State_start = [],
%%     Number_of_processes = 1,
%%     Start_position = [{5,5}],
%%     State = spawn_people(State_start,  Number_of_processes, {X_max, Y_max}, Start_position),
%%     [{PID, Status, X, Y}] = State,
%%     ?assertEqual(length(State), Number_of_processes),
%%     ?assert(is_pid(PID)),
%%     ?assertEqual(Status, 0),
%%     ?assertEqual([{X,Y}], Start_position).
        
%% %%
%% %% Testing spawn_people when several (10) processes are spawned. 
%% %%
%% spawn_people_several_test() ->
%%     Bounds = {10,10},
%%     State_start = [],
%%     Number_of_processes = 10,
%%     Starting_positions = [{X,Y} || X <- lists:seq(1,Number_of_processes), Y <- lists:seq(1,Number_of_processes), X == Y],
%%     State = spawn_people(State_start, Number_of_processes, Bounds, Starting_positions),
%%     ?assertEqual(length(State), Number_of_processes),
%%     New_positions = lists:map(fun({_,{_,X,Y}}) -> {X,Y} end, State),
%%     ?assertEqual(Starting_positions, New_positions).

%%
%% Testing people by sending ready message. 
%%
people_ready_test() ->
    register(master, self()),
    {X_max, Y_max} = {10,10},
    Person = {0,9,0},
    PID = spawn(fun() -> people(Person, {X_max, Y_max}) end),
    PID ! ready,
    receive
        {work, {P, S, X, Y}} ->
            ?assertEqual(P, PID),
            ?assertEqual(S, 0),
            ?assert(0 =< X),
            ?assert(X =< X_max),
            ?assert(0 =< Y),
            ?assert(Y =< Y_max)                
    end,
    unregister(master).

%%
%% Testing people by sending stop message. 
%%
people_stop_test() ->
   ok.

%%
%% Testing people by sending stop message. 
%%
people_ready_and_stop_test() ->
   ok.
