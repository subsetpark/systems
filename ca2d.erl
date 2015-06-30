-module(ca2d).
-export([process_state/2, make_state/2, render/1, get_start/0]).

-spec process_state(grids:grid(), [ca:rule()]) -> grids:grid().
process_state(State, Rules) ->
    process_state(State, Rules, grids:new_grid(array:size(State)), 0).
process_state(State, Rules, Out, N) ->
    case round(math:pow(array:size(State), 2)) of
        N -> Out;
        _ -> Neighborhood = grids:get_neighborhood(N, State),
             R = ca:apply_rules(Neighborhood, Rules),
             process_state(State, Rules, grids:set_cell(N, Out, R), N+1)
    end.

make_state(Type, N) ->
    NewGrid = grids:new_grid(N),
    case Type of
        origin ->
            grids:set_cell(N * N div 2, NewGrid, 1);
        random ->
            random:seed(os:timestamp()),
            F = fun(K, Grid) ->
                        Value = random:uniform(2) - 1,
                        grids:set_cell(K, Grid, Value)
                end,
            lists:foldl(F, NewGrid, lists:seq(0, N * N))
    end.

render(Grid) ->
    io:format("\e[H\e[J"),
    array:map(fun(_, Value) -> render_row(Value) end, Grid),
    timer:sleep(100),
    ok.
render_row(Row) ->
    Rendered = array:map(fun(_, Value) -> ca:render_cell(Value) end, Row),
    io:format("~p~n", [array:to_list(Rendered)]).

get_start() -> {{bits, 5}, {make_state, fun make_state/2}}.
