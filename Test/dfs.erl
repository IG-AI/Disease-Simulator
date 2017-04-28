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
%% @doc DFS Algorithm
%%
%% <p>For examples you can check the <code>dfs_demo</code> module.</p>
%% 

-module(dfs).

-export([run/2]).

-type stack()   :: [{graph:vertex(), term()}].
-type states()  :: dict:dict().
-type parents() :: dict:dict().

%% ----------------------------------------------------------
%% DFS Abstractions
%% ----------------------------------------------------------

%% Stack Abstractions
-define(EMPTY_STACK(), []).
-define(IS_EMPTY(S), S =:= []).
-define(ADD_TO_STACK(Node, Cost, S), [{Node, Cost}|S]).
-define(REMOVE_FROM_STACK(S), {hd(S), tl(S)}).
%% States Abstractions
%% State 'A' : not visited
%% State 'Y' : explored but not added to the result set
%% State 'E' : explored and added to result set
-define(EMPTY_STATES(), dict:new()).
-define(SET_STATE(Node, State, States), dict:store(Node, State, States)).
-define(GET_STATE(Node, States), dict:fetch(Node, States)).
%% Parents Abstractions
-define(EMPTY_PARENTS(), dict:new()).
-define(ADD_TO_PARENTS(Node, Cost, Prev, R), dict:store(Node, {Cost, Prev}, R)).

%% ==========================================================
%% Exported Functions
%% ==========================================================

%% @doc Runs the DFS algorithm on a graph <code>Graph</code>
%% with <code>Root</code> as point of origin
-spec run(graph:graph(), graph:vertex()) -> [graph_lib:path_info()].
run(Graph, Root) ->
  {S, M, P, Vertices} = dfs_init(Graph, Root),
  Result = dfs_step(Graph, S, M, P),
  graph_lib:reconstruct_all_paths(Vertices, Result).
  
%% ==========================================================
%% BFS Functions
%% ==========================================================

%% Initialize data structures
-spec dfs_init(graph:graph(), graph:vertex()) -> {stack(), states(), parents(), [graph:vertex()]}.
dfs_init(Graph, Root) ->
  Ms = ?EMPTY_STATES(),
  S = ?ADD_TO_STACK(Root, 0, ?EMPTY_STACK()),
  Ps = ?ADD_TO_PARENTS(Root, 0, root, ?EMPTY_PARENTS()),
  Vs = graph:vertices(Graph),
  NMs = lists:foldl(fun(V, M) -> ?SET_STATE(V, 'A', M) end, Ms, Vs),
  {S, NMs, Ps, Vs}.
  
%% DFS loop
-spec dfs_step(graph:graph(), stack(), states(), parents()) -> parents().
dfs_step(Graph, S, M, P) ->
  case ?IS_EMPTY(S) of
    true -> P;
    false ->
      {{V, Cost}, NS} = ?REMOVE_FROM_STACK(S),
      NM = ?SET_STATE(V, 'Y', M),
      Neighbours = graph:out_neighbours(Graph, V),
      {NxtP, NxtS} = 
        lists:foldl(
          fun(U, {FP, FS}) ->
            case ?GET_STATE(U, NM) of
              'A' ->
                W = graph:edge_weight(Graph, {V, U}),
                NCost = Cost + W,
                SS = ?ADD_TO_STACK(U, NCost, FS),
                PP = ?ADD_TO_PARENTS(U, NCost, V, FP),
                {PP, SS};
              _ ->
                {FP, FS}
            end
          end,
          {P, NS}, Neighbours),
      NxtM = ?SET_STATE(V, 'E', M),
      dfs_step(Graph, NxtS, NxtM, NxtP)
  end.

