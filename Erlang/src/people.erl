-module(people).

-export([spawn_people_path/5, generate_direction/0]).


-include("includes.hrl").

%%
%% @doc Spawn the people that will be used in the simulation. 
%%
%% @param State The current state that you would like to add people to. Will be empty most of the time.
%% @param Amount The amount of people that you would like to spawn.
%% @param X_max the upper bound on the x-axis
%% @param Y_max the upper bound on the y-axis
%% @param X the x-coordinate for the next process to be spawned, must be smaller than X_max and larger than 0
%% @param Y the y-coordinate for the next process to be spawned, must be smaller than Y_max and larger than 0
%% @param Positions a list with starting positions for the people. Should be the same length as Amount.
%% @param Life How many 'ticks' a process will continue for after being infected
%% 
%% @returns A state with Amount number of people with their status set to 0. 
%%
-spec spawn_people(State :: state(), Amount :: integer(), Bounds :: bounds(), [position()], Life :: non_neg_integer()) -> state().
spawn_people(State, 0, _, _, _) ->
    State;

spawn_people(State, Amount, {X_max, Y_max},[{X,Y} | Positions], Life) ->
    Direction = generate_direction(),
    PID = spawn(fun() -> people({?HEALTHY, X,Y, Direction}, {X_max,Y_max}, Life) end),
    spawn_people(State ++ [{PID, ?HEALTHY, X,Y}], Amount-1, {X_max,Y_max}, Positions, Life).	    

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
%% @doc Loop untill it receives the atom stop. The process will update X and Y with a new random position
%% and send a tagged tuple with its new position and its pid to the registred processes master if it receives the atom ready. If a process becomes infected it will die after Life number of 'ticks' 
%% 
%% @param S the new state of the person. Representing the health of the person.
%% @param X the new x coordinate of the person.
%% @param Y the new y coordinate of the person.
%% @param Bounds the limits of X and Y.
%% @param Life How many 'ticks' a process will continue for after being infected
%%
%% @returns done
%%
-spec people(person(), Bounds :: bounds(),Life :: non_neg_integer()) -> done.
people({S,X,Y,Direction}, Bounds, Life) ->
    receive
        ready ->           
            {X_new, Y_new, Direction_new} = movement:new_position(X,Y,Direction,Bounds), % Get new position          
            master ! {work, {self(), S, X_new, Y_new},Life},
            if
               % S == ->
                 S == ?INFECTED->
                    people({S, X_new, Y_new, Direction_new}, Bounds, Life-1); %if process infected, decrease Life
                true -> 
                    people({S, X_new, Y_new, Direction_new}, Bounds, Life)
            end;
      
        {infect_people, Probability, Targets} ->
            [PID ! get_infected || (rand:uniform())=<Probability, PID <- Targets], % Try to infect processes in its proximity
           
            people({S, X, Y, Direction}, Bounds, Life-1);   
            
           

        get_infected ->
            people({?INFECTED, X, Y, Direction}, Bounds, Life); %Change status to infected

        stop ->
            done
    end.

 

parse(S) ->
    {_, [_,{S1, L1}, {S2, L2}]} = re:run(S, "([0-9]+)[^0-9]*([0-9]+)"),
    {A,_} = string:to_integer(string:substr(S, S1+1, L1)),
    {B,_} = string:to_integer(string:substr(S, S2+1, L2)),
    {A,B}.

%-spec spawn_people_path(State :: state(), Amount :: integer(), Bounds :: bounds(), [position()], Life :: non_neg_integer()) -> state().
spawn_people_path(State, 0, _, _, _) ->
    State;

spawn_people_path(State, Amount, Map_name, Bounds, Life) ->
   
    F = fun({X1, Y1}, {X2, Y2}) -> abs(X1 - X2) + abs(Y1 - Y2) end, %%%%NOT OURS
    G = graph:import("data/"++Map_name++".adjmap", fun parse/1), %%%%%NOT OURS
    %io:format("HALLO~p~n", [G]),
    spawn_people_aux(State, Amount, Map_name, Bounds, Life, G, F).

spawn_people_aux(State, 0, _, _, _, _, _) ->
    State;

spawn_people_aux(State, Amount, Map_name, Bounds, Life, G, F) ->
    [P1, P2, P3] = movement:generate_start_positions(3, Bounds, []),
    Result_1 =  a_star:run(G, P1, P2, F),
    %{_, Path_1} = a_star:run(G, {1,1}, {5,5}, F),
    Result_2 = a_star:run(G, P2, P3, F),
     Result_3 = a_star:run(G, P3, P1, F),
    case (Result_1 =:= unreachable) orelse (Result_2 =:= unreachable) orelse (Result_3 =:= unreachable) of
        true ->
            spawn_people_aux(State, Amount, Map_name, Bounds, Life, G, F);
        false->
            {_, Path_1} = Result_1,
            {_, Path_2} = Result_2,
            {_, Path_3} = Result_3,
            Paths = Path_1 ++ (Path_2 ++ Path_3),
                                                %io:format("PATH:~p~n",[Path_1]),
            PID = spawn(fun() -> people_path(?HEALTHY, Map_name, Paths, 0, length(Paths), Life) end),
            [{X , Y} | _ ] = Paths,
            spawn_people_aux(State ++ [{PID, ?HEALTHY, X, Y}], Amount-1,  Map_name, Bounds, Life, G, F)
    end.




status_check(Status) ->
%    Path_change = (Path_counter rem Path_length).
if
   Status == ?INFECTED -> 1;
   true -> 0
end.





%-spec people_path(integer(), [integer()], {position(),position(),position()}, integer()) -> ok.
people_path(Status, Map, Paths, Path_counter, Paths_length, Life) ->
    receive
        ready ->
            {X,Y} = lists:nth(Path_counter+1, Paths),
            master ! {work, {self(), Status, X, Y},Life},                                     
            people_path(Status, Map, Paths, ((Path_counter+1) rem Paths_length), Paths_length, Life - status_check(Status));
             

         {infect_people, Probability, Targets} ->
            [PID ! get_infected || (rand:uniform())=<Probability, PID <- Targets], % Try to infect processes in its proximity
           
            people_path(Status, Map, Paths, Path_counter, Paths_length, Life-1);               
           

        get_infected ->
            people_path(?INFECTED, Map, Paths, Path_counter, Paths_length, Life); %Change status to infected

        stop ->
            done 
    end, 
                
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% A* (not ours)%%%%%%%%%%%%%%%%%%%%%%%%%%5
%% The representation of a vertex.
-type my_vertex() :: {integer(), integer()}.

%% Parses the string that holds a vertex.
%% -spec parse_vertex(string()) -> my_vertex().
%% parse_vertex([$(, X, $,, Y, $)]) -> {X - $0, Y - $0}.


 

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
