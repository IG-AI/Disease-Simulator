-module(main).
-export([start/0]).

-include_lib("eunit/include/eunit.hrl").
-include("includes.hrl").

%%
%% @doc The main function for starting the Erlang part of the simulation. It will read 3 arguments from the command line: A name of a bitmap file
%% which will be the background for the graphical representation of the simulation, how many people that will be in the simulation, and how many ticks 
%% that the simulation should run for. It then tries to connect to the Java server, if it successfully connects it will request information about the map
%% from the Java server. It then starts the simulation if it receives an afirmative response. 
%%
%% @returns true if the map information was received, false otherwise
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
%% @param State the state of the simulation
%% @param Times The amount of 'ticks' the simulation shall run for
%% @param Java_connection the information used to send messages to the Java server
%% @param Range an offset used to calculate the area in which a process can infect others
%% @param Posibility the posibility that a process will be infected (between 0 and 1)
%%
%% @returns the State at the end of the simulation
%%
-spec master(State :: state(), Times :: integer(), Java_connection :: {atom(),atom()}, Range :: non_neg_integer(), Probability :: float(),  End :: integer()) -> no_return().
master(State, 0, Java_connection, _, _, _) ->
    unregister(master), %remove master from the list of named processes 
    utils:send_to_all(stop, State), %send ending signal to all proccesses in State
    Java_connection ! {simulation_done}, %send ending signal to Java server
    io:format("Simulation ending ~n");

master(State, Times, Java_connection, Range, Probability, End) ->     
    master_call_all(State), %send starting message to all processes in State
    receive
        {result, New_state} ->  

	receive 
            ready_for_positions ->
                %%io:format("Got position request...\n"),             
                Java_connection ! {updated_positions, New_state}, %send new state to the java server                
                Infected = calculate_targets(New_state, Range, Probability),
                case endstate(New_state, Infected, End) of
                    true ->
                        master(New_state, 0, Java_connection, Range, Probability, End);
                    false ->	
                        master(New_state, Times-1, Java_connection, Range, Probability, End)
                end
        end
            
    end.


%%
%% @doc Check if an endcondition have been reached
%%
%% @param State the state of the simulation
%% @param Infected a list of all the infected processes
%% @param End a switch indicating which end condition should be used
%%
%% @returns true it an end condition has been reched, false otherwise 
%%
endstate(State, Infected, End) ->
    if 
        End == ticks ->
            false;        
	State == [] ->
	    io:format("All processes are dead ~n"),
	    true;
	(Infected == []) and ((End == dead) or (End == infected))->
	    io:format("All processes are healthy ~n"),
	    true;
	(length(Infected) == length(State)) and (End == infected) ->
	    io:format("All processes are infected ~n"),
	    true;
	true ->
	    false
    end.


%%
%% @doc Divides State into two lists: one for all the infected process and one for all the healthy processes, 
%% and then calls calculate_target_aux 
%%
%% @param State the state of the simulation
%% @param Range an offset used to calculate the area in which a process can infect others
%% @param Probability the probability that a process will be infected
%%
%% @results a list of all infected processes
%%
-spec calculate_targets(State :: state(), Range :: non_neg_integer(), Probability :: float()) -> state().
calculate_targets(State, Range, Probability) ->
    Infected = [{PID, S, X ,Y} || {PID, S , X ,Y} <- State, S =:= ?INFECTED], % Put all infected processes into a list
    Healthy = [{PID, S, X ,Y} || {PID, S , X ,Y} <- State, S =:= ?HEALTHY], % Put all healthy processes into a list
    calculate_targets_aux(Infected, Healthy, Range, Probability),
    Infected.

%%
%% @doc Compares the head of Infected with each process in Healthy and send a message to the infected process
%%  to infect the healthy processes that are close to it.  
%%
%% @param PID the process ID of the current infected process
%% @param X the x coordinate of the current infected process
%% @param Y the y coordinate of the current infected process
%% @param Infected a list of all the infected processes
%% @param Healthy a list of all the healthy processes
%% @param Range an offset used to calculate the area in which a process can infect others
%% @param Probability the probability of a process being infected
%%
%% @returns done
%%
-spec calculate_targets_aux(Infected :: state() , Healthy :: state(), Range :: non_neg_integer(), Probability :: float()) -> done.
calculate_targets_aux([], _, _, _) ->
    done;

calculate_targets_aux([{PID, _, X, Y} | Infected], Healthy, Range, Probability) ->
    Target_list = [PID_target || {PID_target, _, X_target, Y_target} <- Healthy, 
                                 ((X >= X_target-Range) 
                                  andalso (X =< X_target+Range)
                                  andalso (Y >= Y_target-Range) 
                                  andalso (Y =< Y_target+Range))], % Put all healthy processes that are within range squares into a list
    case Target_list of
        [] ->
            ok;
        _ ->
            PID ! {infect_people, Probability, Target_list} % Send list to the infected process
    end,
    calculate_targets_aux(Infected, Healthy, Range, Probability).
    


%%
%% @doc Sends a message to each process in State and tells it to uppdate its position. It then waits for a message from each process to ensure that each
%% process have uppdated its position
%%
%% @param State the state of the simulation
%%
%% @returns returns the state with the updated positions
%%
-spec master_call_all(State :: state()) -> no_return().
master_call_all(State) ->
    utils:send_to_all(ready, State),
    utils:wait_fun(State, master, length(State)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
