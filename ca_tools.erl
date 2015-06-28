-module(ca_tools).
-export([iterate/3, make_rule/2, make_state/1, random_state/1, render_cell/1]).

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
