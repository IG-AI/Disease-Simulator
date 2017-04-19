-module(map_handler).

-include("includes.hrl").

-export([get_map/2]).

get_map(JavaConnection, Map) ->
    % Use provided connection to ask the server for information
    % regarding the specified map.
	JavaConnection ! {self(), 'map_please', Map},
	io:fwrite("Waiting for map.\n"),
	receive % We got something!
		{_, here_is_map, Width, Height, Walls, Hospital} ->
			io:fwrite("Got map!\n"),
			{Width, Height, Walls, Hospital}
	after 30000 ->  % If no responce in 30 seconds we give up.
		io:fwrite("Got no map =(\n"),
		false
	end.
