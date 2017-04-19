-module(people).
-export([spawn_people/4, people/2]).

-include("includes.hrl").

-spec spawn_people(State :: state(), Amount :: integer(), Bounds :: bounds(), [position()]) -> state().
spawn_people(State, 0, _, _) ->
    State;

spawn_people(State, Amount, {X_max, Y_max}, [{X,Y} | Positions]) ->
    S = 0,  
    PID = spawn(fun() -> people({S,X,Y}, {X_max,Y_max}) end),
    %spawn_people([{PID,{S,X,Y}} | State], N-1, {X_max,Y_max}, Positions).
    spawn_people(State ++ [{PID,{S,X,Y}}], Amount-1, {X_max,Y_max}, Positions).

-spec people(person(), Bounds :: bounds()) -> any().
people({S,X,Y}, Bounds) ->
    receive
        ready ->           
            {X_new, Y_new} = movement:new_rand_position(X,Y,Bounds),           
            master ! {work, {self(), {S,X_new,Y_new}}},            
            people({S, X_new, Y_new}, Bounds);          
        stop ->
            done
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%T esting spawn_people when no processes are spawned. 
%%
spawn_people_none_test() ->
    Bounds = {10,10},
    State_start =[],
    Start_position = [{5,5}],
    State = spawn_people(State_start, 0, Bounds, Start_position),
    ?assertEqual(State_start, State).

%%
%% Testing spawn_people when a single process is spawned. 
%%
spawn_people_single_test() ->
    {X_max, Y_max} = {10,10},
    State_start = [],
    Number_of_processes = 1,
    Start_position = [{5,5}],
    State = spawn_people(State_start,  Number_of_processes, {X_max, Y_max}, Start_position),
    [{PID, {Status, X, Y}}] = State,
    ?assertEqual(length(State), Number_of_processes),
    ?assert(is_pid(PID)),
    ?assertEqual(Status, 0),
    ?assertEqual([{X,Y}], Start_position).
        
%%
%% Testing spawn_people when several (10) processes are spawned. 
%%
spawn_people_several_test() ->
    Bounds = {10,10},
    State_start = [],
    Number_of_processes = 10,
    Starting_positions = [{X,Y} || X <- lists:seq(1,Number_of_processes), Y <- lists:seq(1,Number_of_processes), X == Y],
    State = spawn_people(State_start, Number_of_processes, Bounds, Starting_positions),
    ?assertEqual(length(State), Number_of_processes),
    New_positions = lists:map(fun({_,{_,X,Y}}) -> {X,Y} end, State),
    ?assertEqual(Starting_positions, New_positions).

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
        {work, {P, {S, X, Y}}} ->
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
