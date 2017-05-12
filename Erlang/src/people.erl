-module(people).
-export([spawn_people/5]).
-include("includes.hrl").

%%
%% @doc Spawn the people that will be used in the simulation. 
%%
%% @param State The current state that you would like to add people to. Will be empty most of the time.
%% @param Amount The amount of people that you would like to spawn.
%% @param Movement_behaviour The behaviour the individuals will have when moving.
%% @param Starting_life How many 'ticks' an individual will continue to run after being infected
%% @param Map_info Information about the map
%%
%% @returns A state with Amount number of people with their status set to healthy. 
%%
-spec spawn_people(State :: state(), Amount :: integer(), Movement_behaviour :: atom(), Starting_life :: non_neg_integer(), Map_info :: 
{[integer()], non_neg_integer(), non_neg_integer(), map(), map()}) -> state().
spawn_people(State, 0, _, _, _) ->
    State;

spawn_people(State, Amount, path, Starting_life, Map_info) ->
    {Map_name, Width, Height, Walls, Hospital} = Map_info,
    adj_map:adj_map(Map_name, {Width, Height, Walls, Hospital}),                    
    F = fun({X1, Y1}, {X2, Y2}) -> abs(X1 - X2) + abs(Y1 - Y2) end, %%%%NOT OURS
    G = graph:import("data/"++Map_name++".adjmap", fun parse/1), %%%%%NOT OURS    
    spawn_people_aux(State, Amount, Map_name, {Width, Height}, Starting_life, G, F);

spawn_people(State, Amount, Movement_behaviour, Starting_life, Map_info) ->
    {_, X_max, Y_max, _, _} = Map_info, 
    Direction = movement:generate_direction(),
    {X, Y} = movement:generate_position({X_max, Y_max}),
    PID = spawn(fun() -> people(?HEALTHY, Starting_life, Starting_life, Movement_behaviour, {{X, Y}, Direction, {X_max, Y_max}}) end),
    spawn_people(State ++ [{PID, ?HEALTHY, X, Y}], Amount-1, Movement_behaviour, Starting_life, Map_info).

spawn_people_aux(State, 0, _, _, _, _, _) ->
    State;

spawn_people_aux(State, Amount, Map_name, Bounds, Starting_life, G, F) ->
    [P1, P2, P3] = [movement:generate_position(Bounds), movement:generate_position(Bounds), movement:generate_position(Bounds)],
    Result_1 =  a_star:run(G, P1, P2, F),
    Result_2 = a_star:run(G, P2, P3, F),
    Result_3 = a_star:run(G, P3, P1, F),
    case (Result_1 =:= unreachable) orelse (Result_2 =:= unreachable) orelse (Result_3 =:= unreachable) of
        true ->
            spawn_people_aux(State, Amount, Map_name, Bounds, Starting_life, G, F);
        false->
            {_, Path_1} = Result_1,
            {_, Path_2} = Result_2,
            {_, Path_3} = Result_3,
            Paths = Path_1 ++ (Path_2 ++ Path_3),           
            PID = spawn(fun() -> people(?HEALTHY, Starting_life, Starting_life, path, {Paths, []}) end),
            [{X , Y} | _ ] = Paths,
            spawn_people_aux(State ++ [{PID, ?HEALTHY, X, Y}], Amount-1,  Map_name, Bounds, Starting_life, G, F)
    end.


%%
%% @doc Loop untill it receives the atom stop. The process will update X and Y with a new random position
%% and send a tagged tuple with its new position and its pid to the registred processes master if it receives the atom ready. If a process becomes infected it will die after Life number of 'ticks' 
%% 
%% @param Status the new state of the person. Representing the health of the person.
%% @param Life How many 'ticks' an individual will continue to run after being infected.
%% @param Starting_life How much Life the individual had when it spawned
%% @param Movement_behaviour The behaviour the individuals will have when moving.
%% @param Movement_args The arguments the individuall will use when moving
%%
%% @returns done
%%
-spec people(Status :: status(), Life :: non_neg_integer(), Starting_life :: non_neg_integer(), Bounce_behaviour :: atom(), Movement_args :: {position(), direction(), bounds()} | {pos_list(), pos_list()}) -> done.
people(Status, Life, Starting_life, path, {[], Paths_visited}) ->
    people(Status, Life, Starting_life, path, {lists:reverse(Paths_visited),[]});


people(Status, Life, Starting_life, Movement_behaviour, Movement_args) ->
    receive
        ready ->
            case Movement_behaviour of
                path ->
                    {Paths, Paths_visited} = Movement_args,
                    [{X_new, Y_new}|Remaining_paths] = Paths,
                    New_movement_args = {Remaining_paths, [{X_new, Y_new} | Paths_visited]};
                _ ->
                    {{X, Y}, Direction, Bounds} = Movement_args,
                    case Movement_behaviour of
                        bounce ->
                            {X_new, Y_new, Direction_new} = movement:new_bounce_position(X,Y,Direction,Bounds); % Get new position          
                        bounce_random->
                            {X_new, Y_new, Direction_new} = movement:new_bounce_random_position(X,Y,Direction,Bounds)   
                    end,
                    New_movement_args = {{X_new, Y_new}, Direction_new, Bounds}
            end,

            master ! {work, {self(), Status, X_new, Y_new}, Life},      

            case(collision_checker:get_hospital_location(X_new,Y_new)) of
                true ->	                   
                    people(?IMMUNE, Life, Starting_life, Movement_behaviour, New_movement_args);
                _ ->                       
                    people(Status, Life - status_check(Status), Starting_life, Movement_behaviour, New_movement_args) %if process infected, decrease Life
            end;

        {infect_people, Probability, Targets} ->
            [PID ! get_infected || (rand:uniform())=<Probability, PID <- Targets], % Try to infect processes in its proximity

            people(Status, Life, Starting_life, Movement_behaviour, Movement_args);            


        get_infected ->
            case Status of
                ?IMMUNE ->
                    people(Status, Life, Starting_life, Movement_behaviour, Movement_args);
                _ ->
                    people(?INFECTED, Life, Starting_life, Movement_behaviour, Movement_args) %Change status to infected
            end;
        stop ->
            done
    end.


parse(S) ->
    {_, [_,{S1, L1}, {S2, L2}]} = re:run(S, "([0-9]+)[^0-9]*([0-9]+)"),
    {A,_} = string:to_integer(string:substr(S, S1+1, L1)),
    {B,_} = string:to_integer(string:substr(S, S2+1, L2)),
    {A,B}.


status_check(Status) ->
if
   Status == ?IMMUNE -> 0;
   Status == ?INFECTED -> 1;
   true -> 0
end.
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
%% people_ready_test() ->
%%     register(master, self()),
%%     {X_max, Y_max} = {10,10},
%%     Person = {0,9,0},
%%     PID = spawn(fun() -> people(Person, {X_max, Y_max}) end),
%%     PID ! ready,
%%     receive
%%         {work, {P, S, X, Y}} ->
%%             ?assertEqual(P, PID),
%%             ?assertEqual(S, 0),
%%             ?assert(0 =< X),
%%             ?assert(X =< X_max),
%%             ?assert(0 =< Y),
%%             ?assert(Y =< Y_max)                
%%     end,
%%     unregister(master).

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
