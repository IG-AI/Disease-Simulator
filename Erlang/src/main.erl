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
-spec start() -> ok.
start() -> 
  
    % Setting the java servers' information
    Java_connection_string = {'java_server', 'java_server@localhost'},
    % Handling arguments sent through command line.
    Args = init:get_plain_arguments(),
    % The map file is sent through command line.
    [Map, S_amount, S_times] = Args,
    Amount = list_to_integer(S_amount), 
    Times = list_to_integer(S_times), 
    %Here we start up the net thingy
    java_connection:initialise_network(),

    %Connect to the java server and get the server PID
    Java_connection = java_connection:connect_java(Java_connection_string, 15),

    case Java_connection of
        false -> timer:sleep(10);	%failed connection

        _ ->	% We could connect to the java server
            case map_handler:get_map(Java_connection_string, Map) of	% check if we get information about a map
                {Width, Height, Walls, Hospital} ->	% information about map aquired

                    % Dump information about the newly read map.
                    io:format("Width: ~p, Height: ~p\n", [Width, Height]),	
                    io:format("Map: ~p\n", [Walls]),
                    io:format("Hospital: ~p\n", [Hospital]),
                    Start_positions = movement:generate_start_positions(Amount, {Width ,Height}, []),  %generate starting positions for people processes
                    State  = people:spawn_people([], Amount, {Width, Height}, Start_positions),  %spawn people processes
                    master(State, Times, Java_connection_string); %start master

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
%%
%% @returns 
%%
-spec master(State :: state(), Times :: integer(), Java_connection :: {[integer()],[integer()]}) -> state().
master(State, 0, Java_connection) ->
    unregister(master), %remove master from the list of named processes 
    utils:send_to_all(stop,State), %send ending signal to all proccesses in State
    Java_connection ! {simulation_done}, %send ending signal to Java server
    State;

master(State, Times, Java_connection) ->     
    master_call_all(State), %send starting message to all processes in State
    receive
        {result, New_state} ->  

	receive 
            ready_for_positions ->
                io:format("Got position request...\n"),
                Java_formatted_state  = [{PID,S,X,Y} || {PID,{S,X,Y}} <- State], %reformat the state to fit the frontend 
                Java_connection ! {updated_positions, Java_formatted_state}, %send new state to the java server 
                master(New_state, Times-1, Java_connection)
        end
            
    end.

%%
%% @doc Sends a message to each process in State and tells it to uppdate its position. It then waits for a message from each process to ensure that each
%% process have uppdated its position
%%
%% @param State the state of the simulation
%%
%% @returns returns the state with the updated positions
%%
-spec master_call_all(State :: state()) -> state().
master_call_all(State) ->
    utils:send_to_all(ready, State),
    Result = utils:wait_fun(State, master, length(State)),
    Result.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
