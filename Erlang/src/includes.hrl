%% Includes %%

-type person() :: {status(),integer(),integer()}.
-type state() :: [{pid(),person()}].
-type status() :: integer().
-type bounds() :: {integer(), integer()}.
-type position() :: {integer(), integer()}.

-include_lib("eunit/include/eunit.hrl").

