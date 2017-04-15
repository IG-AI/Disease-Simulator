-module(main).
-export([start/3]).

-type person() :: {status(),integer(),integer()}.
-type state() :: [{pid(),person()}].
-type status() :: integer().
-type bounds() :: {integer(), integer()}.
-type position() :: {integer(), integer()}.

-include_lib("eunit/include/eunit.hrl").

-spec start(Amount :: integer(),Times :: integer(), Bounds :: bounds()) -> state().
start(Amount, Times, Bounds) ->   
    Start_positions = generate_start_positions(Amount,Bounds,[]),
    State  = spawn_people([],Amount, Bounds,Start_positions),  
    register(master, self()),
    master(State, Times).

-spec generate_start_positions(Amount :: integer(), bounds(), [position()]) -> [position()].
generate_start_positions(0,_,Result) ->
    Result;

generate_start_positions(Amount, {X_max,Y_max}, Result) ->
    generate_start_positions(Amount-1, {X_max, Y_max},[{rand:uniform(X_max), rand:uniform(Y_max)} | Result]).


-spec master(State :: state(), Times :: integer()) -> state().
master(State, 0) ->
    unregister(master),
    send_to_all(stop,State),
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
    send_to_all(ready, State),
    Result = master_wait(State, length(State)),
    Result.

-spec master_wait(State :: state(), Num :: integer()) -> {result,state()}.
master_wait(State, Num) ->
    wait_fun(State, master, Num).

-spec wait_fun(State :: state(), Receiver :: pid(), Num :: integer()) -> state().
wait_fun(State, Receiver, 0) ->  
    Receiver ! {result, State},
    State;

wait_fun(State, Receiver, Num) ->
    receive
       {work, {PID, Value}} ->
            New_state = lists:keyreplace(PID, 1, State, {PID,Value}),
            wait_fun(New_state, Receiver, Num-1)
    end.

%% -spec spawn_people(State :: state(), N :: integer(), Bounds :: bounds()) -> state().
%% spawn_people(State, 0, _) ->
%%     State;

%% spawn_people(State, N, {Xmax, Ymax}) ->
%%     S = 0,
%%     X = rand:uniform(Xmax),
%%     Y = rand:uniform(Ymax),
%%     PID = spawn(fun() -> people({S,X,Y}, {Xmax,Ymax}) end),
%%     spawn_people([{PID,{S,X,Y}} | State], N-1, {Xmax,Ymax}).


-spec spawn_people(State :: state(), N :: integer(), Bounds :: bounds(), [position()]) -> state().
spawn_people(State, 0, _, _) ->
    State;

spawn_people(State, N, {X_max, Y_max}, [{X,Y} | Positions]) ->
    S = 0,  
    PID = spawn(fun() -> people({S,X,Y}, {X_max,Y_max}) end),
    %spawn_people([{PID,{S,X,Y}} | State], N-1, {X_max,Y_max}, Positions).
    spawn_people(State ++ [{PID,{S,X,Y}}], N-1, {X_max,Y_max}, Positions).

-spec people(person(), Bounds :: bounds()) -> any().
people({S,X,Y}, Bounds) ->
    receive
        ready ->           
            {X_new, Y_new} = new_rand_position(X,Y,Bounds),           
            master ! {work, {self(), {S,X_new,Y_new}}},            
            people({S, X_new, Y_new}, Bounds)          
    end.

-spec new_position(X :: integer(), Y :: integer(), Position :: integer()) -> {integer(),integer()}.
new_position(X, Y, Position) ->
    case Position of 
        1 ->
            {X-1, Y+1};
        2 ->
            {X, Y+1};
        3 ->
            {X+1, Y+1};
        4 ->
            {X-1, Y};
        5 ->
            {X, Y};
        6 ->
            {X+1, Y};
        7 ->
            {X-1, Y-1};
        8 ->
            {X, Y-1};
        9 ->
            {X+1, Y-1}
        end.

-spec new_rand_position(X :: integer(), Y :: integer(), bounds()) -> {integer(),integer()}.
new_rand_position(X, Y, {X_max, Y_max}) ->
    {X_new, Y_new} = new_position(X, Y, rand:uniform(9)),
    validate_position(X, Y, X_new, Y_new, {X_max, Y_max}).
    

-spec validate_position(X_old :: integer(), Y_old :: integer(), X_new :: integer(), X_new :: integer(), bounds()) -> {integer(),integer()}.
validate_position(X_old, Y_old, X_new, Y_new, {X_max, Y_max}) ->
    if
        X_new > X_max ->
            {X_old, Y_old};
        X_new < 0 ->
            {X_old, Y_old};
        Y_new > Y_max ->
            {X_old, Y_old};
        Y_new < 0 -> 
            {X_old,Y_old};
        true -> 
            {X_new, Y_new}
    end.


%% validate_position_bool(X, Y, {X_max, Y_max}) ->
%%     if
%%         X > X_max ->
%%             false;
%%         X < 0 ->
%%             false;
%%         Y > Y_max ->
%%              false;
%%         Y < 0 -> 
%%              false;
%%         true -> 
%%             true
%%     end.


-spec send_to_all(Msg :: term(),state()) -> ok.
send_to_all(_, []) ->
    ok;

send_to_all(Msg, [{PID,_} | Elems]) ->
    PID ! Msg,
    send_to_all(Msg, Elems).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Testing generate_start_positions when no positions are generated.
%%
genarate_start_positions_zero_test() ->
    Bounds = {10,10},
    Number_of_positions = 0,
    Start_1 = [],
    Result_1 = generate_start_positions(Number_of_positions, Bounds, Start_1),
    ?assertEqual(Start_1, Result_1),
    Start_2 = [{1,1},{2,2},{3,3}],
    Result_2 = generate_start_positions(Number_of_positions, Bounds, Start_2),
    ?assertEqual(Start_2, Result_2).

%%
%% Testing generate_start_positions when one position is generated.
%%
genarate_start_positions_one_test() ->
    {X_max, Y_max} = {10,10},
    Number_of_positions = 1,
    Start = [],
    Result = generate_start_positions(Number_of_positions, {X_max,Y_max}, Start),
    [{X,Y}] = Result,
    ?assertEqual(length(Result), Number_of_positions),
    ?assert(X =< X_max),
    ?assert(X >= 0),
    ?assert(Y =< Y_max),
    ?assert(Y >= 0).

%%
%% Testing generate_start_positions when several (10) positions are generated.
%%
genarate_start_positions_several_test() ->
    {X_max, Y_max} = {10,10},
    Number_of_positions = 10,
    Start = [],
    Result = generate_start_positions(Number_of_positions, {X_max,Y_max}, Start),
    ?assertEqual(length(Result), Number_of_positions).

%%
%% Testing new_position by testing all outcomes.
%%
new_position_test() ->
     [?assertEqual({9,11},new_position(10,10,1)),
      ?assertEqual({10,11},new_position(10,10,2)),
      ?assertEqual({11,11},new_position(10,10,3)),
      ?assertEqual({9,10},new_position(10,10,4)),
      ?assertEqual({10,10},new_position(10,10,5)),
      ?assertEqual({11,10},new_position(10,10,6)),
      ?assertEqual({9,9},new_position(10,10,7)),
      ?assertEqual({10,9},new_position(10,10,8)),
      ?assertEqual({11,9},new_position(10,10,9))].

%%
%% Testing valid_possition when X or Y is within bounds. 
%%
validate_position_within_bounds_test() ->
    Bounds = {10, 10},
    {X_start, Y_start} = {5, 5},
    {X_valid, Y_valid} = {7, 7},

    Position = validate_position(X_start, Y_start, X_valid, Y_valid, Bounds),
    ?assertEqual(Position, {X_valid, Y_valid}).

%%
%% Testing valid_possition when X or Y is out of bounds. 
%%
validate_position_out_of_bounds_test() ->
    Bounds = {10, 10},
    {X_start, Y_start} = {5, 5},
    {X_valid, Y_valid} = {7, 7},
    
    % Case 1: when X is larger than the upper bound                                                
    X_over = 11,   
    Case_1 = validate_position(X_start, Y_start, X_over, Y_valid, Bounds),
    ?assertEqual(Case_1, {X_start, Y_start}),

    % Case 2: when X is smaller than the lower bound                                                
    X_under = -1,
    Case_2 = validate_position(X_start, Y_start, X_under, Y_valid, Bounds),
    ?assertEqual(Case_2, {X_start, Y_start}),

    % Case 3: when Y is larger than the upper bound                                          
    Y_over = 11,
    Case_3 = validate_position(X_start, Y_start, X_valid, Y_over, Bounds),
    ?assertEqual(Case_3, {X_start, Y_start}),

    % Case 4: when Y is smaller than the lower bound 
    Y_under = -1,
    Case_4 = validate_position(X_start, Y_start, X_valid, Y_under, Bounds),
    ?assertEqual(Case_4, {X_start, Y_start}).

%%
%% Testing the edge cases (X or Y on Bounds) of valid_position.  
%%
validate_position_edge_cases_test() ->
    Bounds = {10, 10},
    {X_start, Y_start} = {5, 5},
    {X_valid, Y_valid} = {7, 7},
    
    % Case 1: when X is on than the upper bound                                                
    X_on_upper = 10,   
    Case_1 = validate_position(X_start, Y_start, X_on_upper, Y_valid, Bounds),
    ?assertEqual(Case_1, {X_on_upper, Y_valid}),

    % Case 2: when X is on than the lower bound                                                
    X_on_lower = 0,
    Case_2 = validate_position(X_start, Y_start, X_on_lower, Y_valid, Bounds),
    ?assertEqual(Case_2, {X_on_lower, Y_valid}),   

    % Case 3: when Y is on than the upper bound                                          
    Y_on_upper = 10,
    Case_3 = validate_position(X_start, Y_start, X_valid, Y_on_upper, Bounds),
    ?assertEqual(Case_3, {X_valid, Y_on_upper}),

    % Case 4: when Y is on than the lower bound 
    Y_on_lower = 0,
    Case_4 = validate_position(X_start, Y_start, X_valid, Y_on_lower, Bounds),
    ?assertEqual(Case_4, {X_valid, Y_on_lower}).


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
%% Testing wait_fun by running it once and changing one element.
%%
wait_fun_test() ->
    SELF = self(),
    Test_state = [{SELF,{0,0,0}}],
    PID = spawn(fun() -> wait_fun(Test_state,SELF,1) end ),
    PID ! {work,{SELF,{1,1,1}}},
    receive
        {result,State} ->
            ?assertEqual([{SELF,{1,1,1}}],State)
    after 1000 ->
            ?assert(false)
    end.

%%
%% Testing wait_fun when no message arrives
%%
wait_fun_2_test() ->
    SELF = self(),
    Test_state = [{SELF,{0,0,0}}],
    PID = spawn(fun() -> wait_fun(Test_state,SELF,1) end ),
    PID ! {nowork,{SELF,{1,1,1}}},
    receive
        _ ->
            ?assert(false)
    after 500 ->
            ?assertEqual([{SELF,{0,0,0}}],Test_state)
    end.

%%
%% Trying to change two elements but only one will change because we only run it once
%%         
wait_fun_3_test() ->                       
    SELF = self(),
    Processes = [{spawn(fun() -> true end),{0,0,0}} || _ <- lists:seq(1,10)],
    [{Test_process_1_pid,Test_process_1_data} | [{Test_process_2_pid,Test_process_2_data} | _]] = Processes,
    PID = spawn(fun() -> wait_fun(Processes,SELF,1) end ),
    PID ! {work,{Test_process_1_pid,{1,1,1}}},
    PID ! {work,{Test_process_2_pid,{1,1,1}}},
    receive
        {result, [Actuall_1 | [Actuall_2 | _]]} ->
            ?assertNotEqual(Actuall_1,{Test_process_1_pid,Test_process_1_data}),
            ?assertEqual(Actuall_2,{Test_process_2_pid,Test_process_2_data})
    after 1000 ->
            ?assert(false)
    end.

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
    after 1000 ->
            ?assert(false)
    end.

%%
%% Test send to all with an empty list.
%%
send_to_all_empty_test() ->
    ?assertEqual(ok,send_to_all(ok,[])).
