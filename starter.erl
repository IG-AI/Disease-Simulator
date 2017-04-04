-module(starter).
-export([start/0]).

start() ->
	% Setting the java servers' information
	JavaConnectionString = {'java_server', 'java_server@localhost'},
	
	% Handling arguments sent through command line.
	Args = init:get_plain_arguments(),
	% The map file is sent through command line.
	Map = hd(Args),

	%Here we start up the net thingy
	initialise_network(),

	%Connect to the java server and get the server PID
	JavaConnection = connect_java(JavaConnectionString, 15),

	case JavaConnection of
		false -> w(10);	%failed connection

		_ ->	% We could connect to the java server
			case get_map(JavaConnectionString, Map) of	% check if we get information about a map
				{Width, Height, Walls, Hospital} ->	% information about map aquired
					
					% Dump information about the newly read map.
					io:format("Width: ~p, Height: ~p\n", [Width, Height]),	
					io:format("Map: ~p\n", [Walls]),
					io:format("Hospital: ~p\n", [Hospital]);
				_ ->	% No map information =(
					w(10)	%just to do something..
			end
	end,
	w(10).	%just to do something..

initialise_network() ->
	%Startup node
	net_kernel:start([erlang_starter@localhost, shortnames]),

	% Cookies allows us to communicate only with 
	% others with the same secret.
	% We want to run the following code, but it might fail
	% if run immediately if the node is not created yet.
	% Therefor cookie() checks if node is created and then
	% set the cookie.
	% erlang:set_cookie(node(), secret),	
	cookie(false),

	% if we register the process we can recieve messages
	% sent to the named process
	register(erlang_starter, self()).


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

% Waits for the node to get started properly
% and then set the cookie! Omnomnom! =O
cookie(true) ->
	erlang:set_cookie(node(), secret);
cookie(_) ->
	w(100), %small delay just because..
	cookie(is_alive()).


% Used to wait.. and less chars to type.. =)
w(N) ->
    timer:sleep(N).

% Connects to the provided java server.
% Timeout is the number of seconds we'll try before we give up..
connect_java(Name, Timeout) ->
	io:format("Java server: ~p\n", [Name]),
	io:fwrite("Connecting to Java"),	
	connect_java_aux(Name, Timeout).

% No connection =(
connect_java_aux(_, Timeout) when Timeout =< 0 -> 
	io:format("\nConnection to Java failed.\n"),
	false;
	
% Try to connect...
connect_java_aux(Name, Timeout) ->
	Name ! {self(), 'ping'},    % we send a ping
	receive %And hopefully receive a pong together with the JavaPid.
		{JavaPid, pong} ->
			io:format("\nConnected to Java on Pid: ~p\n", [JavaPid]),
			JavaPid % Return JavaPid
	after 1000 ->   % Tick.. tock...
		io:fwrite("."),
		connect_java_aux(Name, Timeout - 1)
	end.
	
