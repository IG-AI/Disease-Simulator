-module(main).
-export([start/0]).

-include_lib("eunit/include/eunit.hrl").
-include("includes.hrl").

%%
%% @doc The main function for starting the Erlang part of the simulation. It will read 9 arguments from the command line: 
%% A name of a bitmap file which should be the background for the graphical representation of the simulation,
%% the number of individuals that should be in the simulation, 
%% the number of ticks that the simulation should run for, 
%% the number of individuals that should be infected at the start of the simulation,
%% the range in which individuals can infect eachother,
%% the probability of an infection occuring,
%% the starting life of the individuals,
%% the behaviour that individuals should have when moving,
%% and the end condition of the simulation
%% It then tries to connect to the Java server, if it successfully connects it will request information about the map
%% from the Java server. It then starts the simulation if it receives an afirmative response. 
%%
%% @returns true if the map information was received, false otherwise.
%%
-spec start() -> no_return().
start() -> 
  
    % Setting the java servers' information
    Java_connection_string = {'java_server', 'java_server@localhost'},
    % Handling arguments sent through command line.
    Args = init:get_plain_arguments(),
    % The map file is sent through command line.
    [Map, S_amount, S_times, S_nr_of_infected, S_range, S_probability, S_life, S_movement, S_end] = Args,
    Amount = list_to_integer(S_amount), 
    Times = list_to_integer(S_times), 
    Nr_of_infected = list_to_integer(S_nr_of_infected),
    Range = list_to_integer(S_range),
    Probability = list_to_float(S_probability),
    Life = list_to_integer(S_life),
    Movement = list_to_atom(S_movement),
    End = list_to_atom(S_end),

    %Here we start up the net thingy
    java_connection:initialise_network(),

    %Connect to the java server and get the server PID
    Java_connection = java_connection:connect_java(Java_connection_string, 15),

    case Java_connection of
        false -> timer:sleep(10);	%failed connection

        _ ->	% We could connect to the java server
            case map_handler:get_map(Java_connection_string, Map++".bmp") of	% check if we get information about a map
                {Width, Height, Walls, Hospital} ->	% information about map aquired                   

		    register(checker, spawn(fun() -> collision_checker:check_wall(Walls) end)),
		    register(h_checker, spawn(fun() -> collision_checker:check_hospital(Hospital) end)),
                  
                    State = people:spawn_people([], Amount, Movement, Life, {Map, Width, Height, Walls, Hospital}),
                    Infect_list = lists:sublist(State, Nr_of_infected),
                    utils:send_to_all(get_infected, Infect_list),
                    
                    master(State, Times, Java_connection_string, Range, Probability, End); %start master
                

                _ ->	% No map information =(
                    false	%just to do something..
            end,          
            true	    

    end.


%%
%% @doc Start a simulation that will send a new state to the Java server after each tick.
%% 
%% @param State The state of the simulation.
%% @param Ticks The amount of 'ticks' the simulation shall run for.
%% @param Java_connection The information used to send messages to the Java server.
%% @param Range An offset used to calculate the area in which an individual can infect others.
%% @param Posibility The posibility that an individual will be infected (between 0 and 1).
%% @param End The end condition of the simulation.
%%
-spec master(State :: state(), Ticks :: integer(), Java_connection :: {atom(),atom()}, Range :: non_neg_integer(), Probability :: float(),  End :: atom()) -> no_return().
master(State, 0, Java_connection, _, _, _) ->
    unregister(master), %remove master from the list of named processes 
    utils:send_to_all(stop, State), %send ending signal to all proccesses in State
    Java_connection ! {simulation_done}, %send ending signal to Java server
    io:format("Simulation ending ~n");

master(State, Ticks, Java_connection, Range, Probability, End) ->     
    master_call_all(State), %send starting message to all processes in State
    receive
        {result, New_state} ->  

	receive 
            ready_for_positions ->                
                Java_connection ! {updated_positions, New_state}, %send new state to the java server                
                Infected_list = calculate_targets(New_state, Range, Probability), %infect individuals
                case endstate(New_state, Infected_list, End) of %check if an endstate have been reached
                    true ->
                        master(New_state, 0, Java_connection, Range, Probability, End);
                    false ->	
                        master(New_state, Ticks-1, Java_connection, Range, Probability, End)
                end
        end
            
    end.


%%
%% @doc Check if an endcondition have been reached.
%%
%% @param State The state of the simulation.
%% @param Infected_list A list of all the infected individuals.
%% @param End The end condition of the simulation.
%%
%% @returns true if an end condition has been reached, false otherwise. 
%%
-spec endstate(state(),state(),atom()) -> boolean().
endstate(State, Infected_list, End) ->
    if 
        End == ticks ->
            false;        
	State == [] ->
	    io:format("All processes are dead ~n"),
	    true;
	(Infected_list == []) and ((End == dead) or (End == infected))->
	    io:format("All processes are healthy ~n"),
	    true;
	(length(Infected_list) == length(State)) and (End == infected) ->
	    io:format("All processes are infected ~n"),
	    true;
	true ->
	    false
    end.


%%
%% @doc Divides State into two lists: one for all the infected individual and one for all the healthy individuals, 
%% and then calls calculate_target_aux. 
%%
%% @param State The state of the simulation.
%% @param Range An offset used to calculate the area in which a individual can infect others.
%% @param Probability The probability that a individual will be infected.
%%
%% @returns A list of all the infected individuals.
%%
-spec calculate_targets(State :: state(), Range :: non_neg_integer(), Probability :: float()) -> state().
calculate_targets(State, Range, Probability) ->
    Split = fun  
                Split([{PID, ?HEALTHY, X, Y} | Rest], I, H) ->
                    Split(Rest, I, [{PID, ?HEALTHY, X, Y} | H]);
                
                Split([{PID, ?INFECTED, X, Y} | Rest], I, H) ->
                    Split(Rest, [{PID, ?INFECTED, X, Y} | I], H);
                
                Split([{_, ?IMMUNE, _, _} | Rest], I, H) ->
                    Split(Rest,I,H);
                
                Split([], I, H) -> 
                    {I, H}  
            end,
    {Infected_list, Healthy_list} = Split(State, [], []),
    calculate_targets_aux(Infected_list, Healthy_list, Range, Probability),
    Infected_list.


%%
%% @doc Compares the head of Infected_list with each individual in Healthy and sends a message to the infected individual saying that it should
%% try to infect the healthy individuals that are close to it.  
%%
%% @param PID The individual ID of the current infected individual.
%% @param X The x coordinate of the current infected individual.
%% @param Y The y coordinate of the current infected individual.
%% @param Infected_list A list of all the infected individuals.
%% @param Healthy_list A list of all the healthy individuals.
%% @param Range An offset used to calculate the area in which a individual can infect others.
%% @param Probability The probability of a individual being infected.
%%
-spec calculate_targets_aux(Infected_list :: state() , Healthy_list :: state(), Range :: non_neg_integer(), Probability :: float()) -> no_return().
calculate_targets_aux([], _, _, _) ->
    done;

calculate_targets_aux([{PID, _, X, Y} | Infected_list], Healthy_list, Range, Probability) ->
    Target_list = [PID_target || {PID_target, _, X_target, Y_target} <- Healthy_list, 
                                 ((X >= X_target - Range) 
                                  andalso (X =< X_target + Range)
                                  andalso (Y >= Y_target - Range) 
                                  andalso (Y =< Y_target + Range))], % Put all healthy processes that are within range squares into a list
    case Target_list of
        [] ->
            ok;
        _ ->
            PID ! {infect_people, Probability, Target_list} % Send list to the infected process
    end,
    calculate_targets_aux(Infected_list, Healthy_list, Range, Probability).
    


%%
%% @doc Sends a message to each indivindual in State and tells it to update its position. It then waits for a message from each individual to ensure that each
%% individual have updated its position.
%%
%% @param State the state of the simulation.
%%
%% @returns returns the state with the updated positions.
%%
-spec master_call_all(State :: state()) -> no_return().
master_call_all(State) ->
    utils:send_to_all(ready, State),
    utils:wait_fun(State, master, length(State)).


