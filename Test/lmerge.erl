-module(lmerge).
-export([lmerge/0, timer/1]).


-spec lmerge() -> no_return().
lmerge() ->
    timer(fun()-> my_merger(lists:seq(1, 1000, 2), lists:seq(0, 1000*10000, 2)) end ),
    timer(fun()-> my_merger(lists:seq(1, 1000*10000, 2), lists:seq(0, 1000, 2)) end ),

    timer((fun() -> lists:merge(lists:seq(1, 1000, 2), lists:seq(0, 1000*10000, 2)) end )),
    timer((fun() -> lists:merge(lists:seq(1, 1000*10000, 2), lists:seq(0, 1000, 2)) end )),    

    timer((fun()-> lists:seq(1, 1000, 2) ++ lists:seq(0, 1000*10000, 2) end)),
    timer((fun()-> lists:seq(0, 1000*10000, 2) ++ lists:seq(1, 1000, 2) end)).



-spec timer(F::function())->integer().
timer(F) ->
    T = timer_aux(F, 10, 0),
    io:format("TIME: ~p ~n",[T]).

timer_aux(_, 0, Sum) ->
    Sum;

timer_aux(F, N, Sum)->
    B = erlang:timestamp(), 
    F(), 
    A = erlang:timestamp(),
    timer_aux(F, N-1, Sum+timer:now_diff(A,B)).


my_merger([], L2)->
    L2;
my_merger([L1H|L1T], L2) ->
    my_merger(L1T, [L1H|L2]).

    
