-module(ca).
-export([process_state/2]).
-export_type([state/0, rule/0]).

-type cell() :: 0..1.
-type state() :: [cell()].
-type rule() :: {state(), cell()}.

-spec apply_rules(state(), [rule()]) -> cell().
apply_rules([X, Y, Z], [{[X, Y, Z], R}|_]) ->
    R;
apply_rules(N, [_|Rules]) ->
    apply_rules(N, Rules).

-spec process_state(state(), [rule()]) -> state().
process_state(State, Rules) ->
    process_state(State, Rules, [], 1).

-spec process_state(state(), [rule()], state(), non_neg_integer()) -> state().
process_state(State, _,  Out, N) when N > length(State)->
    lists:reverse(Out);
process_state(State, Rules, Out, N) ->
    [X, Y, Z] = [mod_nth(N+K, State) || K <- [-1, 0, 1]],
    R = apply_rules([X, Y, Z], Rules),
    process_state(State, Rules, [R|Out], N+1).

mod_nth(N, L) -> lists:nth(mod(N, length(L)), L).
mod(X, Y) when X == 0; X==Y -> Y;
mod(X, Y) -> (X rem Y + Y) rem Y.
