-module(record).
-export([start_record/0]).


%% @doc Starts the process identified by the atom 'record' messages sent to this process
%% instead of the Java-process will be saved to disk
-spec start_record() -> no_return().
start_record() ->
    register(record, spawn(fun() -> record(proper_time:time_to_string()++".txt", start) end)).

%% @doc record will listen for connections to 'record' and write them to it's supplied filepointer.
%%
%% @param Control Will contain either a filename to be appended to, or a pointer to an open file to write to
%% @param start|wait is used to decide if the process should open a file for writing
%% or wait for incoming messages with data to write
%% 
-spec record(Control :: [non_neg_integer()]|pid(), start|simulation_done|wait) -> no_return().
record(Filename, start)->
    %öppna fil för skrivning
    case file:open(Filename, [append]) of
        {ok, Pointer} ->
            record(Pointer, wait);
        {error, Reason} ->
            io:format("Could not open file: ~p ~n", [Reason])
    end;

record(Pointer, wait) ->
    receive
        {updated_positions, New_state} ->
            %io:fwrite(Pointer, "~w~n", [New_state]),
            case file:write(Pointer, io_lib:fwrite("~p",[New_state])) of %this is probably faster and can handle more data than io:fwrite()
                {error, Reason} ->
                    io:format("Could not write to file: ~p ~n", [Reason]);
                _ ->
                    record(Pointer, wait)
            end;
        {simulation_done, Time_data} ->
            io:fwrite(Pointer, "~p~n", [Time_data]),
            case file:close(Pointer) of
                {error, Reason} ->
                    io:format("Could not close file: ~p ~n", [Reason]);
                _ ->
                    ok
            end
    end.
