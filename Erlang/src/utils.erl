-module(utils).
-export([send_to_all/2, wait_fun/3, generate_start_status/3]).

-include("includes.hrl").

%%
%% @doc Sends a message to all processes in a State
%%
%% @param Msg the message to be relayed 
%% @param PID the process ID of the process that the message will be sent to next 
%% @param Elems the rest of the State
%%
%% @returns ok
%%
-spec send_to_all(Msg :: term(),state()) -> ok.
send_to_all(_, []) ->
    ok;

send_to_all(Msg, [{PID,_,_,_} | Elems]) ->
    PID ! Msg,
    send_to_all(Msg, Elems).  

%%
%% @doc Loops until it have received Num number of 'work' messages. Each message reveived contains a tuple with a PID 
%% and a Value. The element in State that have the corresponding PID will be replaced with the tuple received in the 
%% message. Once Num messages have been reveived the State will be sent to Receiver. 
%%
%% @param State the state of the simulation
%% @param Receiver the PID of the process that the State will be sent to once all messages have been received
%% @param Num the number of messages to be received before the function returned
%%
%% @returns The State once all messages have been received
%%
-spec wait_fun(State :: state(), Receiver :: pid(), Num :: integer()) -> state().
wait_fun(State, Receiver, 0) ->  
    Receiver ! {result, State},
    State;

wait_fun(State, Receiver, Num) ->
    receive
        {work,{PID, _, _, _}, 0} -> %If the Life of the process is 0 a stop message is sent to the process
            PID ! stop,
            New_state = lists:delete(PID, 1, State), %remove the process from State
            wait_fun(New_state, Receiver, Num-1);
        {work, {PID, S, X, Y},_} ->
            New_state = lists:keyreplace(PID, 1, State, {PID, S, X, Y}), 
            wait_fun(New_state, Receiver, Num-1)
    end.

%%
%% @doc Generate Amount number of integers, the first Nr_of_infected are 1 the others are 0
%%
%% @param Amount the amount of statuses to be generated
%% @param Nr_of_infected the amount of infected people to start with
%% @param Result the list to which the new statuses are to be appended
%%
%% @returns Result with the statuses appended to it
%%
%-spec generate_start_status(Amount :: non_neg_integer(),Nr_of_infected :: non_neg_integer(), Result :: [0 | 1]) -> [0 | 1].
generate_start_status(0, _, Result) ->
    Result;

generate_start_status(Amount,0,Result) ->
    generate_start_status(Amount-1, 0, [?HEALTHY | Result]);

generate_start_status(Amount, Nr_of_infected, Result) ->
    generate_start_status(Amount-1, Nr_of_infected-1, [?INFECTED | Result]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%
%% Test send_to_all with a none empty list and check that each processes in the list received the message
%%
send_to_all_test() ->
    Self = self(),
    PID = spawn(fun () ->
                        receive 
                            test -> 
                                Self ! ok
                        end
                end),
    ok =  send_to_all(test, [{PID,{0,0,0}}]),
    receive 
        ok -> 
            ?assert(true)
    after 400 ->
            ?assert(false)
    end.

%%
%% Test send to all with an empty list.
%%
send_to_all_empty_test() ->
    ?assertEqual(ok,send_to_all(ok,[])).


%%
%% Testing wait_fun by running it once and changing one element.
%%
wait_fun_test() ->
    Self = self(),
    Test_state = [{Self,{0,0,0}}],
    PID = spawn(fun() -> wait_fun(Test_state,Self,1) end ),
    PID ! {work,{Self,{1,1,1}}},
    receive
        {result,State} ->
            ?assertEqual([{Self,{1,1,1}}],State)
    after 1000 ->
            ?assert(false)
    end.

%%
%% Testing wait_fun when no message arrives
%%
wait_fun_2_test() ->
    Self = self(),
    Test_state = [{Self,{0,0,0}}],
    PID = spawn(fun() -> wait_fun(Test_state,Self,1) end ),
    PID ! {nowork,{Self,{1,1,1}}},
    receive
        _ ->
            ?assert(false)
    after 1000 ->
            ?assertEqual([{Self,{0,0,0}}],Test_state)
    end.

%%
%% Trying to change two elements but only one will change because we only run it once
%%         
wait_fun_3_test() ->                       
    Self = self(),
    Processes = [{spawn(fun() -> true end),{0,0,0}} || _ <- lists:seq(1,10)],
    [{Test_process_1_pid,Test_process_1_data} | [{Test_process_2_pid,Test_process_2_data} | _]] = Processes,
    PID = spawn(fun() -> wait_fun(Processes,Self,1) end ),
    PID ! {work,{Test_process_1_pid,{1,1,1}}},
    PID ! {work,{Test_process_2_pid,{1,1,1}}},
    receive
        {result, [Actuall_1 | [Actuall_2 | _]]} ->
            ?assertNotEqual(Actuall_1,{Test_process_1_pid,Test_process_1_data}),
            ?assertEqual(Actuall_2,{Test_process_2_pid,Test_process_2_data})
    after 1000 ->
            ?assert(false)
    end.
