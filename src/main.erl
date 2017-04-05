-module(main).
-export([start/3]).

-type person() :: {status(),integer(),integer()}.
-type state() :: [{pid(),person()}].
-type status() :: integer().
-type bounds() :: {integer(), integer()}.

-include_lib("eunit/include/eunit.hrl").

-spec start(Num :: integer(),Times :: integer(), Bounds :: bounds()) -> ok.
start(Num, Times, Bounds) ->   
    State  = spawn_people([],Num, Bounds),   
    register(master, self()),
    master(State, Times),
    ok.

-spec master(State :: state(), Times :: integer()) -> state().
master(State, 0) ->
    unregister(master),
    %%io:format("___________________________________ ~n"),
    State;

master(State, Times) ->     
    master_call_all(State),
    receive
        {result, New_state} ->      
      %%      io:format("State: ~p ~n", [New_state]),
            master(New_state, Times-1)
    end.

-spec master_call_all(State :: state()) -> {result,state()}.
master_call_all(State) ->
    send_to_all(ready, State),
    master_wait(State, length(State)).

-spec master_wait(State :: state(), Num :: integer()) -> {result,state()}.
master_wait(State, Num) ->
    wait_fun(State, master, Num).

-spec wait_fun(State :: state(), Receiver :: pid(), Num :: integer()) -> ok.
wait_fun(State, Receiver, 0) ->  
    Receiver ! {result, State},
    ok;

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
    X =  rand:uniform(Xmax),
    Y = rand:uniform(Ymax),
    PID = spawn(fun() -> people({S,X,Y}, {Xmax,Ymax}) end),
    spawn_people([{PID,{S,X,Y}} | State], N-1, {Xmax,Ymax}).

-spec people(person(), Bounds :: bounds()) -> any().
people({S,X,Y}, Bounds) ->
    receive
        ready ->           
            {X2, Y2} = new_rand_position(X,Y,Bounds),           
            master ! {work, {self(), {S,X2,Y2}}},            
            people({S,X2,Y2}, Bounds)
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
    [?assertEqual(new_position(10,10,1),{9,11}),
     ?assertEqual(new_position(10,10,2),{10,11}),
     ?assertEqual(new_position(10,10,3),{11,11}),
     ?assertEqual(new_position(10,10,4),{9,10}),
     ?assertEqual(new_position(10,10,5),{10,10}),
     ?assertEqual(new_position(10,10,6),{11,10}),
     ?assertEqual(new_position(10,10,7),{9,9}),
     ?assertEqual(new_position(10,10,8),{10,9}),
     ?assertEqual(new_position(10,10,9),{11,9})].

spawn_people_test() ->
    ?assertEqual(length(spawn_people([],5,{10,10})),5).
        
start_test() ->
    State = start(5,5,{10,10}),
    ?assertEqual(length(State), 5).
    
    
































%% rek(P, 0) ->
%%     done3;

%% rek(P,T) ->
%%     io:format("Result ~p: ~p ~n", [T,rec(P,0)]),
%%     rek(P,T-1).

%% rec(0,Acc) ->
%%    Acc;

%% rec(Value,Acc) ->
%%     rec(Value -1,Acc + rand:uniform(50)).
