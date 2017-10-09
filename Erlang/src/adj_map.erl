-module(adj_map).
-export([adj_map/2]).
-include("includes.hrl").

%%
%% @doc This function will generate a new adjacency map for the supplied map if it's not up to date, else it won't do anything.
%%
%% @param Map_name is the name of the map to handle
%% @param Map contains the world information for the supplied map
%%
%% @returns true if the adjacency map was up to date, else false.
%% 
-spec adj_map(Map_name :: [non_neg_integer()], Map :: world()) -> boolean().
adj_map(Map_name, Map) ->
    case check_file(Map_name) of 
        false ->          
            {X_max, Y_max, _Walls, _Hospital} = Map,
            Mod = 1,
            {Pos, Mov} = row(X_max-Mod, Y_max-Mod, {X_max-Mod, Y_max-Mod}, [], [], []),
            file:write_file("data/"++Map_name++".adjmap",  io_lib:fwrite("~p ~p undirected d~n~s ~n~s ~n",[length(Pos), length(Mov), pos_str(Pos,[]), pos_str(Mov, [])])),
            false;
        _ ->
            true
    end.

%%
%% @doc This function checks if the adjacency map for the supplied map exist or and is up to date.
%% An adjacency map is considered up to date if it's last modified date is not older than the corresponding map.
%%
%% @param Map_name the name of the map to check
%%
%% @returns true if the adjacency map for the supplied file exist and is up to date, else false
-spec check_file(Map_name :: [non_neg_integer()]) -> boolean().
check_file(Map_name) ->    
    case filelib:is_regular("data/"++Map_name++".adjmap") of
        false ->
            false;
        true ->
            File_date = filelib:last_modified("data/"++Map_name++".adjmap"),
            Map_date = filelib:last_modified("data/"++Map_name++".bmp"),
            calendar:datetime_to_gregorian_seconds(File_date) >= calendar:datetime_to_gregorian_seconds(Map_date)
    end.


%%
%% @doc Converts adj_list() and pos_list() to readable strings. Put them in Result and recursively call itself until the lists to be converted are empty, then returns Result.
%%
%% @param adj_list | pos_list The adjacency list or position list to be converted
%% @param Result the already converted list.
%% @returns The supplied list converted to a readable string.
%% 
%% @TODO Rewrite the function to two different functions, one taking an adj_list and the other taking a pos_list
-spec pos_str(adj_list() | pos_list(), [[integer()]]) -> [[integer()]].        
pos_str([], Result)->
    Result;

pos_str([{X, Y} | T], Result) ->
    New_str = ["(" ++ integer_to_list(X) ++ "," ++ integer_to_list(Y) ++ ") "],
    pos_str(T, [Result | New_str]);
    
pos_str([{{X1, Y1},{X2, Y2}, Cost} | T], Result) ->
    New_str = ["(" ++ integer_to_list(X1) ++ "," ++ integer_to_list(Y1) ++ ") " ++ 
                   "(" ++ integer_to_list(X2) ++ "," ++ integer_to_list(Y2) ++ ") " ++ integer_to_list(Cost) ++ "\n"],
    pos_str(T, [Result | New_str]).
    


%%
%% @doc Will generate an adjacency map for the active map.
%% The active map will have been read in by the process called by collision_checker:get_wall_collision() and that map's bounds will
%% be supplied to this function.
%%
%% @param X not used
%% @param Y the Y-coordinate row to check
%% @param X_max the highest X-coordinate to use
%% @param Y_max the highest Y-coordinate to use
%% @param Pos the position list that has been generated so far (will continueously be updated with more positions)
%% @param Mov the adjacency list that has been generated so far (will continueously be updated with more positions)
%% @param Prev the valid positions from the previous row
%%
%% @returns A tuple containing valid positions of the map and all valid movements.
%% 
-spec row(X :: integer(), Y :: integer(), bounds(), Pos :: pos_list(), Mov :: adj_list(), Prev :: pos_list()) -> {pos_list(), adj_list()}.
row(_, -1, _, Pos, Mov, _) ->
    {Pos, Mov};

row(X, Y, {X_max, Y_max}, Pos, Mov, Prev) ->
    New_pos = [{X_valid, Y} || X_valid  <- lists:seq(0, X_max), not collision_checker:check_collision(w_checker,X_valid, Y)],

    Check_down = fun (X1, Y1, X2, Y2) -> if
                                             ((X1 =:= X2) and (Y1+1 =:= Y2)) -> true;	% Down
                                             ((X1-1 =:= X2) and (Y1+1 =:= Y2)) -> true;	% Down and left
                                             ((X1+1 =:= X2) and (Y1+1 =:= Y2)) -> true;	% Down and right
                                             true -> false
                                         end
                 end,

    Down = [{{X1,Y1},{X2,Y2}, 1} || {X1,Y1} <- New_pos, {X2,Y2} <- Prev, Check_down(X1, Y1, X2, Y2)],
    Left = [{{X1,Y1},{X2,Y2}, 1} || {X1,Y1} <- New_pos, {X2,Y2} <- New_pos, ((X1+1 =:= X2) and (Y1 =:= Y2))],
    row(X, Y-1, {X_max, Y_max},lists:merge(New_pos, Pos), Down++Left++Mov, New_pos).
