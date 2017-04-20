%% Includes %%

%% @type person(). The persons in the simulation.
-type person() :: {status(),position()}.
%% @type state(). The state of the simulation. 
-type state() :: [{pid(),person()}].
%% @type status(). The status of a persons health.
-type status() :: integer().
%% @type bounds(). The bounds of a map.
-type bounds() :: {integer(), integer()}.
%% @type position. coordinates for a position on the map.
-type position() :: {integer(), integer()}.

-include_lib("eunit/include/eunit.hrl").

