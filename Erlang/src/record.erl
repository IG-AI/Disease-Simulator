-module(record).
-export([start_record/0]).

start_record() ->
    register(record, spawn(fun() -> record(proper_time:time_to_string()++".txt", start) end)).
                    
-spec record(Control :: [non_neg_integer()]|pid(), start|simulation_done|wait) -> no_return().
record(Filename, start)->
    %öppna fil för skrivning
    case file:open("data/"++Filename, [append]) of
        {ok, Pointer} ->
            record(Pointer, wait);
        {error, Reason} ->
            io:format("Could not open file: ~p ~n", [Reason])
    end;

record(Pointer, wait) ->
    receive
        {updated_positions, New_state} ->
            io:fwrite(Pointer, "~w~n", [New_state]),
            record(Pointer, wait);
        {simulation_done, Time_data} ->
            io:fwrite(Pointer, "~p~n", [Time_data]),
            case file:close(Pointer) of
                {error, Reason} ->
                    io:format("Could not close file: ~p ~n", [Reason]);
                _ ->
                    ok
            end
    end.
