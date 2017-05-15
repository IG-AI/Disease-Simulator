-module(record).
-include("includes.hrl").
-export([start_record/2]).


%% @doc Starts the process identified by the atom 'record' messages sent to this process
%% instead of the Java-process will be saved to disk
%% @param Record controls behaviour of the function, and parameters sent to record
%% 
-spec start_record(Record :: atom(), Java_connection :: java_connection()) -> no_return().
start_record(rec, _) ->
    master ! ready_for_positions,
    register(record, spawn(fun() -> record(proper_time:time_to_string()++".record", start, e_master) end));

start_record(play_and_rec, Java) ->
    Java ! {set_up_for_requests},
    register(record, spawn(fun() -> record(proper_time:time_to_string()++".record", start, none) end));

start_record(play, Java) ->  %bg|play
    Java ! {set_up_for_requests},
    register(record, spawn(fun() -> record() end));

start_record(_, _) -> 
    master ! ready_for_positions,
    register(record, spawn(fun() -> record() end)).



record() ->
    receive
        {simulation_done, _} ->
            master ! thanks_for_all_the_fish;
        _ ->
            record()
    end.
%% @doc record will listen for connections to 'record' and write them to it's supplied filepointer.
%%
%% @param Control Will contain either a filename to be appended to, or a pointer to an open file to write to
%% @param start|wait is used to decide if the process should open a file for writing
%% or wait for incoming messages with data to write
%% @param Reply control whom to reply to
%% 
-spec record(Control :: [non_neg_integer()]|pid(), start|simulation_done|wait, Reply :: boolean()) -> no_return().
record(Filename, start, Reply)->
    %öppna fil för skrivning
    case file:open(Filename, [append]) of
        {ok, Pointer} ->
            record(Pointer, wait, Reply);
        {error, Reason} ->
            io:format("Could not open file: ~p ~n", [Reason])
    end;

record(Pointer, wait, Reply) ->
    receive
        {updated_positions, New_state} ->
            %io:fwrite(Pointer, "~w~n", [New_state]),
            case file:write(Pointer, io_lib:fwrite("~p~n",[New_state])) of %this is probably faster and can handle more data than io:fwrite()
                {error, Reason} ->
                    io:format("Could not write to file: ~p ~n", [Reason]);
                _ ->
                    case Reply of
                        e_master ->
                            master ! ready_for_positions ,
                            record(Pointer, wait, Reply);
                        none ->
                            record(Pointer, wait, Reply)
                    end
            end;
        {simulation_done, Time_data} ->
            io:fwrite(Pointer, "~p~n", [Time_data]),
            case file:close(Pointer) of
                {error, Reason} ->
                    io:format("Could not close file: ~p ~n", [Reason]);
                _ ->
                    master ! thanks_for_all_the_fish
            end
    end.
