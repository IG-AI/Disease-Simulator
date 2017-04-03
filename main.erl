-module(main).
-export([get_master/2,spawn_people2/2]).

-type person() :: {integer(),integer(),integer()}.
-type state() :: [{pid(),person()}].

-spec get_master(Num :: integer(),Times :: integer()) -> done.
get_master(Num, Times) ->   
    State  = spawn_people2([],Num),   
    register(master, self()),
    master(State, Times).


master(_, 0) ->
    unregister(master),
    done;


master(State, Times) ->     
    master_call_all(State),
    receive
        {result, NEWState} ->      
            io:format("State: ~p ~n", [NEWState]),
            master(NEWState, Times-1)
    end.

%% get_pos() ->
%%     receive
%%         {work,Person} ->
%%             Person
%%     end.

master_call_all(State) ->
    send_to_all(ready, State),
    master_wait(State, length(State)).


master_wait(State, 0) ->  
    master ! {result, State};
  

master_wait(State, Num) ->
    receive
       {work, {PID, {S,X,Y}}} ->
            New_state = lists:map(fun(C) -> replacement_func(C,{PID,{S,X,Y}}) end, State),
                   
            master_wait(New_state, Num-1)
    end.

replacement_func({PID,X},{PID2,NEW}) ->
    if
        PID =:= PID2 -> {PID,NEW} ;
        true -> {PID,X}
    end.

%% -spec spawn_people(Number :: integer()) -> state().
%% spawn_people(Number) ->
%%     [{spawn(main,fun(X,Y) -> people(X,Y) end,[X,Y]),0,X = rand:uniform(10),Y = rand:uniform(10)} || _ <- lists:seq(1,Number)].




spawn_people2(State, 0) ->
    State;


spawn_people2(State, N) ->
    S = 0,
    X =  rand:uniform(10),
    Y = rand:uniform(10),
    PID = spawn(fun() -> people({S,X,Y}) end),
    spawn_people2([{PID,{S,X,Y}} | State], N-1).

people({S,X,Y}) ->
    receive
        ready ->           
            X2 =  rand:uniform(10),
            Y2 =  rand:uniform(10),           
            master ! {work, {self(), {S,X2,Y2}}},
            
            people({S,X2,Y2})
    end.


send_to_all(_, []) ->
    ok;

send_to_all(Msg, [{PID,_} | Elems]) ->
    PID ! Msg,
    send_to_all(Msg, Elems).  




































%% rek(P, 0) ->
%%     done3;

%% rek(P,T) ->
%%     io:format("Result ~p: ~p ~n", [T,rec(P,0)]),
%%     rek(P,T-1).

%% rec(0,Acc) ->
%%    Acc;

%% rec(Value,Acc) ->
%%     rec(Value -1,Acc + rand:uniform(50)).
