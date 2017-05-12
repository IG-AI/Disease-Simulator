%% Includes %%

-define(HEALTHY,0). %Indicates that an individual have not been infected or vaccinated.
-define(INFECTED,1). %Indicates that an individual have been infected and the disease have started showing symptoms.
-define(IMMUNE,2). %Indicates that an individual have been vaccinated.
-define(INCUBATED,1). %Indicates that an individual have been infected but the disease have not yet started showing symptoms.

%% @type person(). The persons in the simulation.
-type person() :: {status(),position(),direction()}.
%%
-type state_element() :: {pid(), status(), integer(), integer()}.
%% @type state(). The state of the simulation. 
-type state() :: [state_element()].
%% @type status(). The status of a persons health.
-type status() :: integer().
%% @type direction(). The direction a person is moving {X_change, Y_change}.
-type direction() :: {integer(), integer()}.
%% @type bounds(). The bounds of a map. {max X position, max Y position}
-type bounds() :: {integer(), integer()}.
%% @type position(). Coordinates for a position on the map.
-type position() :: {integer(), integer()}.
%% @type java_connection(). The name used to communicate with the Java server.
-type java_connection() :: {atom(),atom()}. 
%% @type world(). Information about the map of the simulation. 
-type world() :: {integer(), integer(), map(), map()}.
%% @type adj_list(). A list from the adjacency map.
-type adj_list() :: [{position(), position(), integer()}].
%% @type pos_list(). A list of x and y coordinates.  
-type pos_list() :: [{position()}].

-include_lib("eunit/include/eunit.hrl").

