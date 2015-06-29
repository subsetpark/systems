-module(ca_tools).
-export([start/4, render_cell/1]).

-type automaton() :: elementary | von_neumann.

-spec start(automaton(), {random | origin, integer()}, integer(), non_neg_integer()) -> ca:state().
start(AutomatonType, {StateType, StateSize}, RuleNumber, Iterations) ->
    State = case StateType of
                origin -> make_state(StateSize);
                random -> random_state(StateSize)
            end,
    Bits = case AutomatonType of
               elementary -> 3;
               von_neumann -> 5
           end,
    Rules = make_rule(RuleNumber, Bits),
    iterate(State, Rules, Iterations).


-spec iterate(ca:state(), [ca:rule()], non_neg_integer()) -> ca:state().
iterate(State, _, 0) ->
    State;
iterate(State, Rules, N) ->
    Generation = ca:process_state(State, Rules),
    render(Generation),
    iterate(Generation, Rules, N - 1).

-spec render(ca:state()) -> ok.
render(S) ->
    io:format("~p~n", [[render_cell(D) || D <- S]]).
render_cell(Cell) when Cell == 0 -> $\s;
render_cell(Cell) when Cell == 1 -> $#.

-spec make_rule(integer(), non_neg_integer()) -> ca:rule().
make_rule(N, Bits) ->
    Max = round(math:pow(2, Bits)),
    Template = [binary_expansion(K, Bits) || K <- lists:seq(Max-1, 0, -1)],
    lists:zip(Template, binary_expansion(N, Max)).

binary_expansion(N, Bits) ->
    Unpadded = hd(io_lib:format("~.2B", [N])),
    Bstring = string:right(Unpadded, Bits, $0),
    F = fun(Digit) ->
                {Int, []} = string:to_integer([Digit]),
                Int
        end,
    lists:map(F, Bstring).

-spec make_state(integer()) -> ca:state().
make_state(N) ->
    lists:duplicate(N div 2, 0) ++ [1] ++ lists:duplicate(N div 2, 0).

-spec random_state(integer()) -> ca:state().
random_state(N) ->
    random:seed(os:timestamp()),
    [random:uniform(2) - 1 || _ <- lists:seq(1, N)].
