-module(main).
-export([start/3]).

-type person() :: {status(),integer(),integer()}.
-type state() :: [{pid(),person()}].
-type status() :: integer().
-type bounds() :: {integer(), integer()}.

-include_lib("eunit/include/eunit.hrl").

-spec start(Num :: integer(),Times :: integer(), Bounds :: bounds()) -> state().
start(Num, Times, Bounds) ->   
    State  = spawn_people([],Num, Bounds),   
    register(master, self()),
    master(State, Times).

-spec master(State :: state(), Times :: integer()) -> state().
master(State, 0) ->
    unregister(master),
    send_to_all(stop,State),
    State;

master(State, Times) ->     
    master_call_all(State),
    receive
        {result, New_state} ->      
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

-spec spawn_people(State :: state(), N :: integer(), Bounds :: bounds()) -> state().
spawn_people(State, 0, _) ->
    State;

spawn_people(State, N, {Xmax, Ymax}) ->
    S = 0,
    X = rand:uniform(Xmax),
    Y = rand:uniform(Ymax),
    PID = spawn(fun() -> people({S,X,Y}, {Xmax,Ymax}) end),
    spawn_people([{PID,{S,X,Y}} | State], N-1, {Xmax,Ymax}).

-spec people(person(), Bounds :: bounds()) -> any().
people({S,X,Y}, Bounds) ->
    receive
        ready ->           
            {X2, Y2} = new_rand_position(X,Y,Bounds),           
            master ! {work, {self(), {S,X2,Y2}}},            
            people({S,X2,Y2}, Bounds);
        stop ->
            {S,X,Y}
    end.

-spec new_position(X :: integer(), Y :: integer(),Position :: integer()) -> {integer(),integer()}.
new_position(X,Y,Position) ->
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
new_rand_position(X, Y, {Xmax, Ymax}) ->
    {X2, Y2} = new_position(X,Y,rand:uniform(9)),
    if
        X2 > Xmax ->
            {X, Y};
        X2 < 0 ->
            {X, Y};
        Y2 > Ymax ->
            {X,Y};
        Y2 < 0 -> 
            {X,Y};
        true -> 
            {X2,Y2}
    end.

-spec send_to_all(Msg :: term(),state()) -> ok.
send_to_all(_, []) ->
    ok;

send_to_all(Msg, [{PID,_} | Elems]) ->
    PID ! Msg,
    send_to_all(Msg, Elems).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%% spawn_people_test() ->
%%     ?assertEqual(length(spawn_people([],5,{10,10})),5).
        
%% start_test() ->
%%     State = start(5,5,{10,10}),
%%     ?assertEqual(length(State), 5).

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
    after 1000 ->
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
    after 3000 ->
            ?assert(false)
    end.

%%
%% Run it two times but only send one message
%%
wait_fun_4_test() ->
    ok.





























