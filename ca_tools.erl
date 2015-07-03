-module(ca_tools).
-export([start/4]).

-type automaton() :: elementary | von_neumann.
-type state_type() :: random | origin.

-spec start(automaton(), {state_type(), integer()}, integer(), non_neg_integer()) -> ca:state().
start(AutomatonType, {StateType, StateSize}, RuleNumber, Iterations) ->
    CaModule = case AutomatonType of 
                   elementary -> ca;
                   von_neumann -> ca2d
               end,
    {{bits, Bits}, {make_state, StateGenerator}} = CaModule:get_start(),
    State = StateGenerator(StateType, StateSize),
    Rules = make_rule(RuleNumber, Bits),
    iterate(CaModule, State, Rules, Iterations).


-spec iterate(module(), ca:state(), [ca:rule()], non_neg_integer()) -> ca:state().
iterate(_, State, _, 0) ->
    State;
iterate(CaModule, State, Rules, N) ->
    Generation = CaModule:process_state(State, Rules),
    CaModule:render(Generation),
    iterate(CaModule, Generation, Rules, N - 1).

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


