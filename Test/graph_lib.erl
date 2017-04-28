%%
%% %CopyrightBegin%
%%
%% Copyright © 2013-2014 Aggelos Giantsios
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy of this software 
%% and associated documentation files (the “Software”), to deal in the Software without restriction, 
%% including without limitation the rights to use, copy, modify, merge, publish, distribute, 
%% sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is 
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included 
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED 
%% TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN 
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% %CopyrightEnd%
%%

%% @copyright 2013-2014 Aggelos Giantsios
%% @author Aggelos Giantsios

%% ============================================================================
%% @doc Library Functions for Graph Algorithms
%%

-module(graph_lib).

-export([reconstruct_all_paths/2, reconstruct_flow/1]).

-export_type([vpath/0, path_info/0, mst/0, mst_info/0, 
              epath/0, epath_weighted/0, flow/0]).

%%
%% @type path_info(). It is used for the result of BFS, DFS 
%% and Dijkstra algorithms.
%% <p>It's a tuple <code>{Vertex, {Cost, Path}}</code> that 
%% contains the information about the <code>Cost :: number()</code>
%% and <code>Path :: vpath()</code> of a <code>Vertex</code>.
%% If a vertex cannot be reached from the root vertex then
%% instead of<code>{Cost, Path}</code> there will be the
%% atom <code>unreachable</code>.</p>
%%
%% @type flow(). It is used for the result of Edmonds-Karp and 
%% Ford-Fulkerson algorithms.
%% <p>It's a tuple <code>{Val, Flow}</code> that contains the information
%% about the value of the flow <code>Val :: number()</code> and how it is
%% achieved by the network's <code>Flow :: [{edge(), number()}]</code>.</p>
%%
-type mst()  :: [graph:edge()].
-type mst_info()  :: {graph:weight(), mst()}.
-type vpath() :: [graph:vertex()].
-type epath() :: [graph:edge()].
-type epath_weighted() :: [{graph:edge(), graph:weight()}].
-type path_info() :: {graph:vertex(), {graph:weight(), vpath()} | 'unreachable'}.
-type flow() :: {graph:weight(), [{graph:edge(), graph:weight()}]}.

%% ==========================================================
%% Exported Functions
%% ==========================================================

%% @doc Reconstruct all the path information from a graph algorithm's result.
%% (Algorithms included: Dijkstra, DFS, BFS).
-spec reconstruct_all_paths([graph:vertex()], dict:dict()) -> [path_info()].

reconstruct_all_paths(Vertices, Result) ->
  SortedVs = lists:sort(fun erlang:'<'/2, Vertices),
  lists:map(fun(V) -> reconstruct_path(Result, V) end, SortedVs).
  
%% @doc Reconstruct the flow information for a flow algortihm's result.
%% (Algorithms included: Edmonds-Karp, Ford-Fulkerson).
-spec reconstruct_flow([proplists:property()]) -> flow().

reconstruct_flow(L) ->
  Flow = proplists:get_value('flow', L),
  Es = lists:sort(L -- [{'flow', Flow}]),
  {Flow, Es}.
  
%% ==========================================================
%% Internal Functions
%% ==========================================================

%% ----------------------------------------------------------
%% Helper functions to reconstruct a path
%% ----------------------------------------------------------

%% Result :: dict of {Node, {Cost, Prev}}
-spec reconstruct_path(dict:dict(), graph:vertex()) -> path_info().

reconstruct_path(Result, Node) ->
  try dict:fetch(Node, Result) of
    {Cost, Prev} ->
      {Node, reconstruct_path(Result, Prev, Cost, [Node])}
  catch
    error:badarg ->
      {Node, 'unreachable'}
  end.

-spec reconstruct_path(dict:dict(), graph:vertex(), term(), vpath()) -> {term(), vpath()}.

reconstruct_path(_Result, root, Cost, Path) ->
  {Cost, Path};
reconstruct_path(Result, Node, Cost, Path) ->
  {_, Prev} = dict:fetch(Node, Result),
  reconstruct_path(Result, Prev, Cost, [Node|Path]).
  

