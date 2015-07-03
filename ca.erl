-module(ca).
-export([process_state/2, apply_rules/2, make_state/2, get_start/0, render/1, render_cell/1]).
-export_type([cell/0, state/0, rule/0]).

-type cell() :: 0..1.
-type state() :: [cell()].
-type rule() :: {state(), cell()}.

-spec apply_rules(Pattern, [{Pattern, Output}]) -> Output.
apply_rules(Neighborhood, [{Neighborhood, R}|_]) ->
    R;
apply_rules(Neighborhood, [_|Rules]) ->
    apply_rules(Neighborhood, Rules).

-spec process_state(state(), [rule()]) -> state().
process_state(State, Rules) ->
    process_state(State, Rules, [], 1).

-spec process_state(state(), [rule()], state(), non_neg_integer()) -> state().
process_state(State, _,  Out, N) when N > length(State)->
    lists:reverse(Out);
process_state(State, Rules, Out, N) ->
    Neighborhood = [mod_nth(N+K, State) || K <- lists:seq(-1, 1)],
    R = apply_rules(Neighborhood, Rules),
    process_state(State, Rules, [R|Out], N+1).

mod_nth(N, L) -> lists:nth(mod(N, length(L)), L).
mod(X, Y) when X == 0; X==Y -> Y;
mod(X, Y) -> (X rem Y + Y) rem Y.

-spec make_state(origin | random, integer()) -> ca:state().
make_state(Type, N) ->
    case Type of
        origin ->
            lists:duplicate(N div 2, 0) ++ [1] ++ lists:duplicate(N div 2, 0);
        random ->
            random:seed(os:timestamp()),
            [random:uniform(2) - 1 || _ <- lists:seq(1, N)]
    end.

-spec render(ca:state()) -> ok.
render(S) ->
        io:format("~p~n", [[render_cell(D) || D <- S]]).
render_cell(Cell) when Cell == 0 -> $\s;
render_cell(Cell) when Cell == 1 -> $#.

get_start() -> {{bits, 3}, {make_state, fun make_state/2}}.
