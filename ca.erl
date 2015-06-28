-module(ca).
-export([process_state/2, apply_rules/2]).
-export_type([cell/0, state/0, rule/0]).

-type cell() :: 0..1.
-type state() :: [cell()].
-type rule() :: {state(), cell()}.

-spec apply_rules(state(), [rule()]) -> cell().
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
