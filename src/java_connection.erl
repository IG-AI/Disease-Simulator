% @author Project Snowfox
% @doc This module provides functionality to connect to a java process
% running on the same machine.

-module(java_connection).

-export([connect_java/2, initialise_network/0]).

-include("includes.hrl").

% @doc Connects to the a java process.
% It will try to connect once every second for the provided amount of time.
% @param Name is the variable used to connect to the process.
% @param Timeout is the number of seconds the function will try
% connecting before giving up.
% @returns Returns the PID of the responding Java process.
% @todo Get the returned PID to work for communication.
-spec connect_java(Name :: {[integer()],[integer()]}, Timeout :: integer()) -> pid()|boolean().
connect_java(Name, Timeout) ->
    io:format("Java server: ~p\n", [Name]),
    io:fwrite("Connecting to Java"),	
    connect_java_aux(Name, Timeout).

% @private
% No connection =(
-spec connect_java_aux(Name :: {[integer()],[integer()]}, Timeout :: integer()) -> pid()|boolean().
connect_java_aux(_, Timeout) when Timeout =< 0 -> 
    io:format("\nConnection to Java failed.\n"),
    false;


% @private
% Try to connect to the java process
connect_java_aux(Name, Timeout) ->
    Name ! {self(), 'ping'},    % we send a ping
    receive % And hopefully receive a pong together with the JavaPid.
        {JavaPid, pong} ->
            io:format("\nConnected to Java on Pid: ~p\n", [JavaPid]),
            JavaPid % Return JavaPid
    after 1000 ->   % Will wait for a reply (pong) 1000ms before trying again.
            io:fwrite("."),
            connect_java_aux(Name, Timeout - 1)
    end.

% @doc Initialise the network part of the code to allow for inter-process communication.
% Will register the process with the name 'master' as an atom.
% The cookie will be set to the atom 'secret'.
-spec initialise_network() -> true.
initialise_network() ->
	% Startup node
	net_kernel:start([master@localhost, shortnames]),

    % Set the cookie.	
	cookie(false),

	% If we register the process we can recieve messages
	% sent to the named process.
	register(master, self()).


% @doc Waits for the node to get started properly and then set the cookie!
% Cookies allows us to communicate only with 
% others with the same secret.
% If the node is not created before we try to set the cookie the code might fail.
% Therefor cookie() checks if node is created and then
% set the cookie.
-spec cookie(boolean()) -> true.
cookie(true) ->
	erlang:set_cookie(node(), secret);
cookie(_) ->
	timer:sleep(100), %small delay before trying to set the cookie again.
	cookie(is_alive()).


