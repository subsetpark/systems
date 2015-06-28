-module(grids).
-export([new_grid/1, get_neighborhood/2, set_cell/3]).
-export_type([grid/0]).

-type grid() :: array:array(ca:cell()).

-spec mod_get_cell(integer(), integer(), grid()) -> ca:cell().
mod_get_cell(X, Y, Grid) ->
    Base = array:size(Grid),
    get_cell(mod(X, Base), mod(Y, Base), Grid).
get_cell(X, Y, Grid) ->
    Row = array:get(Y, Grid),
    array:get(X, Row).
mod(X, Y) when X==Y -> Y-1;
mod(X, Y) -> (X rem Y + Y) rem Y.

set_cell(N, Grid, Value) ->
    {X, Y} = get_coords(N, Grid),
    set_coords(X, Y, Grid, Value).
set_coords(X, Y, Grid, Value) ->
    Row = array:get(Y, Grid),
    NewRow = array:set(X, Value, Row),
    array:set(Y, NewRow, Grid).

get_coords(N, Grid) ->
    CoordString = string:right(integer_to_list(N, array:size(Grid)), 2, $0),
    CoordList = lists:map(fun(D) -> 
                      {Int, []} = string:to_integer([D]),
                      Int
              end, CoordString),
    list_to_tuple(CoordList).

new_array(N, Default) ->
    array:new(N, [{fixed, true}, {default, Default}]).
new_grid(N) ->
    new_array(N, new_array(N, 0)).

-spec get_neighborhood(integer(), grid()) -> [ca:cell()].
get_neighborhood(N, Grid) ->
    {X, Y} = get_coords(N, Grid),
    [mod_get_cell(X2, Y2, Grid) ||
     {X2, Y2} <- [{X, Y}, {X - 1, Y}, {X + 1, Y}, {X, Y -1}, {X, Y + 1}]].


