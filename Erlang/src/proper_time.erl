-module(proper_time).
-export([time_to_string/0]).

add_zero(N) ->
    case N < 10 of
        true ->
            "0" ++ integer_to_list(N);
        _ ->
            integer_to_list(N)
    end.

time_to_string() ->
    {{Year, M, D},{H, I, S}} = calendar:local_time(),
    add_zero(Year)++"_"++add_zero(M)++"_"++add_zero(D)++"_"++add_zero(H)++"_"++add_zero(I)++"_"++add_zero(S).
