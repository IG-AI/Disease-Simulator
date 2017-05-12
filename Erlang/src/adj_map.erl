-module(adj_map).
-export([adj_map/2]).
-include("includes.hrl").


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

%%-spec pos_str(pos_list() | adj_list(), [non_neg_integer()]) -> pos_list() | adj_list().
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
    

%% -spec row(X :: integer(), Y :: integer(), bounds(), Pos :: pos_list(), Mov :: adj_list(), Prev :: pos_list()) -> {pos_list(), adj_list()}.
%% row(_, -1, _, Pos, Mov, _) ->
%%     {Pos, Mov};


%% row(X, Y, {X_max, Y_max}, Pos, Mov, Prev) ->
%%     New_pos = [{X_valid, Y} || X_valid  <- lists:seq(0, X_max), not collision_checker:get_wall_collision(X_valid, Y)],
%%     Down = [{{X1,Y1},{X2,Y2}, 1} || {X1,Y1} <- New_pos, {X2,Y2} <- Prev, ((X1 =:= X2) and (Y1+1 =:= Y2))],
%%     Left = [{{X1,Y1},{X2,Y2}, 1} || {X1,Y1} <- New_pos, {X2,Y2} <- New_pos, ((X1+1 =:= X2) and (Y1 =:= Y2))],
%%     Diagonal_left = [{{X1,Y1},{X2,Y2}, 1} || {X1,Y1} <- New_pos, {X2,Y2} <- Prev, ((X1-1 =:= X2) and (Y1+1 =:= Y2))],
%%     Diagonal_right = [{{X1,Y1},{X2,Y2}, 1} || {X1,Y1} <- New_pos, {X2,Y2} <- Prev, ((X1+1 =:= X2) and (Y1+1 =:= Y2))],
%%     row(X, Y-1, {X_max, Y_max},lists:merge(New_pos, Pos), Down++Left++Diagonal_left++Diagonal_right++Mov, New_pos).



-spec row(X :: integer(), Y :: integer(), bounds(), Pos :: pos_list(), Mov :: adj_list(), Prev :: pos_list()) -> {pos_list(), adj_list()}.
row(_, -1, _, Pos, Mov, _) ->
    {Pos, Mov};

row(X, Y, {X_max, Y_max}, Pos, Mov, Prev) ->
    New_pos = [{X_valid, Y} || X_valid  <- lists:seq(0, X_max), not collision_checker:get_wall_collision(X_valid, Y)],

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
