-module(java_connection).

-export([connect_java/2, initialise_network/0, java_position_test/2]).

-include("includes.hrl").

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


initialise_network() ->
	%Startup node
	net_kernel:start([master@localhost, shortnames]),

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
	register(master, self()).


% Waits for the node to get started properly
% and then set the cookie! Omnomnom! =O
cookie(true) ->
	erlang:set_cookie(node(), secret);
cookie(_) ->
	timer:sleep(100), %small delay just because..
	cookie(is_alive()).



%%
%% Tests
%%
java_position_test(JavaConnection, 0) -> 
    JavaConnection ! {simulation_done};

java_position_test(JavaConnection, I) ->
    receive
	ready_for_positions ->
	    io:format("Got position request...\n"),
	    JavaConnection ! {updated_positions, [
						  {self(), 1, 2, 2},
						  {self(), 2, 3, 4},
						  {self(), 5, 6, 7}
						 ]},
	    java_position_test(JavaConnection, I-1)
end.

