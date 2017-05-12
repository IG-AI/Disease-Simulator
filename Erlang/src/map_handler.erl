-module(map_handler).

-include("includes.hrl").
-export([get_map/2]).

%%
%% @doc Sends the name of a Map file to the Java server and then waits to receive information about the Map. 
%% If the Map information is received the function returns that information. 
%% If no information have been received after 30 seconds, false is returned instead. 
%%
%% @param Java_connection the information used to send messages to the Java server. 
%% @param Map_name the name of a map.
%%
%% @returns a tuple containing the width of the Map, the heigth of the Map, a map of 'wallpixels' and a map of 'hospitalpixels' 
%% if a Map was received, false otherwise.   
%%
-spec get_map(Java_connection :: java_connection(), Map_name :: [integer()]) -> {integer(),integer(),map(),map()} | false.
get_map(Java_connection, Map_name) ->
    % Use provided connection to ask the server for information regarding the specified map.
	Java_connection ! {self(), 'map_please', Map_name},
	io:fwrite("Waiting for map.\n"),
	receive % We got something!
		{_, here_is_map, Width, Height, Walls, Hospital} ->
			io:fwrite("Got map!\n"),
			{Width, Height, Walls, Hospital}
	after 30000 ->  % If no responce in 30 seconds we give up.
		io:fwrite("Got no map =(\n"),
		false
	end.
