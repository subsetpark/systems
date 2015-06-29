-module(ca2d).
-export([process_state/2, random_state/1, render_grid/1]).

-spec process_state(grids:grid(), [ca:rule()]) -> grids:grid().
process_state(State, Rules) ->
    process_state(State, Rules, grids:new_grid(array:size(State)), 1).
process_state(State, Rules, Out, N) ->
    case round(math:pow(array:size(State), 2)) of
        N -> Out;
        _ ->
            Neighborhood = grids:get_neighborhood(N, State),
            R = ca:apply_rules(Neighborhood, Rules),
            process_state(State, Rules, grids:set_cell(N, Out, R), N+1)
    end.

random_state(N) ->
    random:seed(os:timestamp()),
    NewGrid = grids:new_grid(N),
    F = fun(K, Grid) ->
                Value = random:uniform(2) - 1,
                grids:set_cell(K, Grid, Value)
        end,
    lists:foldl(F, NewGrid, lists:seq(0, N * N)).

render_grid(Grid) ->
    array:map(fun(_, Value) -> render_row(Value) end, Grid),
    ok.
render_row(Row) ->
   Rendered = array:map(fun(_, Value) -> ca_tools:render_cell(Value) end, Row),
   io:format("~p~n", [array:to_list(Rendered)]).
