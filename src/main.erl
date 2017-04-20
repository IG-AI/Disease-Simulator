-module(main).
-export([start/0]).

-include_lib("eunit/include/eunit.hrl").
-include("includes.hrl").


%-spec start() -> state().
start() -> 
  
    % Setting the java servers' information
    JavaConnectionString = {'java_server', 'java_server@localhost'},
    % Handling arguments sent through command line.
    Args = init:get_plain_arguments(),
    % The map file is sent through command line.
    [Map,S_amount,S_times] = Args,
    Amount = list_to_integer(S_amount), 
    Times = list_to_integer(S_times), 
    %Here we start up the net thingy
    java_connection:initialise_network(),

    %Connect to the java server and get the server PID
    JavaConnection = java_connection:connect_java(JavaConnectionString, 15),
    %register(java_connection, JavaConnection),
    case JavaConnection of
        false -> timer:sleep(10);	%failed connection

        _ ->	% We could connect to the java server
            case map_handler:get_map(JavaConnectionString, Map) of	% check if we get information about a map
                {Width, Height, Walls, Hospital} ->	% information about map aquired

                    % Dump information about the newly read map.
                    io:format("Width: ~p, Height: ~p\n", [Width, Height]),	
                    io:format("Map: ~p\n", [Walls]),
                    io:format("Hospital: ~p\n", [Hospital]),
                    Start_positions = movement:generate_start_positions(Amount,{Width,Height},[]),
                    State  = people:spawn_people([],Amount, {Width,Height} ,Start_positions),  
                    %register(master, self()),
                    master(State, Times, JavaConnectionString);

                _ ->	% No map information =(
                    ok	%just to do something..
            end,
            ok
            %java_connection:java_position_test(JavaConnectionString, 5)	    

    end.

-spec master(State :: state(), Times :: integer(), Java_connection :: {[integer()],[integer()]}) -> state().
master(State, 0, Java_connection) ->
    unregister(master),
    utils:send_to_all(stop,State),
    Java_connection ! {simulation_done},
    State;

master(State, Times, Java_connection) ->     
    master_call_all(State),
    receive
        {result, New_state} ->  
            %io:format("State: ~p~n",[New_state]),

	receive 
            ready_for_positions ->
                io:format("Got position request...\n"),
                Mamma  = [{PID,S,X,Y} || {PID,{S,X,Y}} <- State],
                Java_connection ! {updated_positions, Mamma},
                master(New_state, Times-1, Java_connection)
        end
            
    end.

-spec master_call_all(State :: state()) -> {result,state()}.
master_call_all(State) ->
    utils:send_to_all(ready, State),
    Result = master_wait(State, length(State)),
    Result.

-spec master_wait(State :: state(), Num :: integer()) -> {result,state()}.
master_wait(State, Num) ->
    utils:wait_fun(State, master, Num).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%
%% Test start 
%%
%% start_test() ->
%%     {X_bound,Y_bound} = {10,10},
%%     Nr_of_processes = 5,
%%     Check = fun ({PID,{S,X,Y}}) -> 
%%                     ?assert(is_pid(PID)),
%%                     ?assert(S =:= 0), 
%%                     ?assert(X >= 0),
%%                     ?assert(X =< X_bound), 
%%                     ?assert(Y >= 0),
%%                     ?assert(Y =< Y_bound)
%%             end,

%%     Result = start(Nr_of_processes,1,{X_bound,Y_bound}),
%%     lists:foreach(Check,Result),
%%     ?assertEqual(Nr_of_processes,length(Result)).

