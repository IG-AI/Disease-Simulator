-module(logging).
-export([time_logger/5]).

-spec time_logger(Args :: [[non_neg_integer()]], Starting_time :: [non_neg_integer()], Startup_time :: erlang:timestamp(), Exec_time :: erlang:timestamp(), Total_time :: erlang:timestamp()) -> no_return().
time_logger(Args, Starting_time, Startup, Exec, Total) ->
    Sts = get_timestamp(Startup),
    Ets = get_timestamp(Exec),
    Tts = get_timestamp(Total),
    Startup_time = Ets - Sts,
    Exec_time = Tts - Ets,
    Total_time = Tts - Sts,
    case file:open("logs/erlang_exec_log.log", [append]) of
        {ok, Pointer} ->
            case file:write(Pointer, io_lib:fwrite("~p~n", [Starting_time])) of
                {error, Reason} ->
                    io:format("Could not write to file: ~p ~n", [Reason]);   
                _ ->
                    case file:write(Pointer, io_lib:fwrite("~p~n", [Args])) of
                        {error, Reason} ->
                            io:format("Could write to file: ~p ~n", [Reason]);   
                        _ ->
                            case file:write(Pointer, io_lib:fwrite("~p ~p ~p~n~n", [Startup_time, Exec_time, Total_time])) of
                                {error, Reason} ->
                                    io:format("Could not write to file: ~p ~n", [Reason]);   
                                _->
                                    case file:open("logs/erlang_exec_time.log", [append]) of
                                        {ok, Time_pointer} ->
                                            case file:write(Time_pointer, io_lib:fwrite("~p ~p ~p~n", [Startup_time, Exec_time, Total_time])) of
                                                {error, Reason} ->     
                                                    io:format("Could not write to file: ~p ~n", [Reason]);   
                                                _ ->
                                                    ok
                                            end;
                                        {error, Reason} ->
                                            io:format("Could not open file: ~p ~n", [Reason])
                                    end
                            end
                    end
            end;
        {error, Reason} ->
            io:format("Could not open file: ~p ~n", [Reason])
    end.

get_timestamp({Mega,Sec,Micro}) ->
    (Mega*1000000+Sec)*1000000+Micro.
