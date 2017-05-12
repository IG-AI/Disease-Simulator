-module(utils).
-export([send_to_all/2, wait_fun/3]).

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
%% @param Num the number of messages to be received before the function returns
%%
%% @returns The State once all messages have been received
%%
-spec wait_fun(State :: state(), Receiver :: pid() | atom(), Num :: integer()) -> no_return().
wait_fun(State, Receiver, 0) ->  
    Receiver ! {result, State},
    State;

wait_fun(State, Receiver, Num) ->
    receive
        {work, {PID, S, X, Y}, Life} ->
            if
                Life =< 0 ->
                   
                    New_state = lists:keydelete(PID, 1, State); %remove the process from State;
                true -> 
                   
                    New_state = lists:keyreplace(PID, 1, State, {PID, S, X, Y}) 
                    
            end,
            wait_fun(New_state, Receiver, Num-1)
    end.


