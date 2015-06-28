-module(tags).
-export([tags/3]).

-type tag() :: [0..1].
-type rule() :: {tag(), tag()}.
-type ruleset() :: [rule()].
-type state() :: [0..1].

-spec tags(ruleset(), state(), integer()) -> state().
tags(_, State, 0) ->
    State;
tags(Rules, State, Iterations) ->
    tags(Rules, run_rules(Rules, State), Iterations - 1).

-spec run_rules([rule()], state()) -> state().
run_rules([], State) ->
    State;
run_rules([Rule|Rules], State) ->
    case match(Rule, State) of
        {true, Ss} ->
            run_rules(Rules, apply_rule(Rule, Ss));
        _ ->
            run_rules(Rules, State)
    end.

-spec match(rule(), state()) -> {true, state()} | false.
match({[P|Ps], Tag}, [P|Ss]) ->
    match({Ps, Tag},  Ss);
match({[], _}, Ss) ->
    {true, Ss};
match(_, _) ->
    false.

-spec apply_rule(rule(), state()) -> state().
apply_rule({_, Tag}, State) ->
    State ++ Tag.
    
