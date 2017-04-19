-module(main).
-export([start/3]).

-include_lib("eunit/include/eunit.hrl").
-include("includes.hrl").

-spec start(Amount :: integer(),Times :: integer(), Bounds :: bounds()) -> state().
start(Amount, Times, Bounds) ->   
    Start_positions = movement:generate_start_positions(Amount,Bounds,[]),
    State  = people:spawn_people([],Amount, Bounds,Start_positions),  
    register(master, self()),
    master(State, Times).


-spec master(State :: state(), Times :: integer()) -> state().
master(State, 0) ->
    unregister(master),
    utils:send_to_all(stop,State),
    State;

master(State, Times) ->     
    master_call_all(State),
    receive
        {result, New_state} ->  
            %io:format("State: ~p~n",[New_state]),
            master(New_state, Times-1)
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
start_test() ->
    {X_bound,Y_bound} = {10,10},
    Nr_of_processes = 5,
    Check = fun ({PID,{S,X,Y}}) -> 
                    ?assert(is_pid(PID)),
                    ?assert(S =:= 0), 
                    ?assert(X >= 0),
                    ?assert(X =< X_bound), 
                    ?assert(Y >= 0),
                    ?assert(Y =< Y_bound)
            end,

    Result = start(Nr_of_processes,1,{X_bound,Y_bound}),
    lists:foreach(Check,Result),
    ?assertEqual(Nr_of_processes,length(Result)).

