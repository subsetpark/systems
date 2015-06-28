-module(ca_tools).
-export([iterate/3, make_rule/1, make_state/1, random_state/1]).

-spec iterate(ca:state(), [ca:rule()], non_neg_integer()) -> ca:state().
iterate(State, _, 0) ->
    State;
iterate(State, Rules, N) ->
    Generation = ca:process_state(State, Rules),
    render(Generation),
    iterate(Generation, Rules, N - 1).

-spec render(ca:state()) -> string().
render(S) ->
    io:format("~p~n", [[render_cell(D) || D <- S]]).
render_cell(Cell) when Cell == 0 -> $\s;
render_cell(Cell) when Cell == 1 -> $#.

-define(RULES, [[1,1,1],[1,1,0],[1,0,1],[1,0,0],[0,1,1],[0,1,0],[0,0,1],[0,0,0]]).

-spec make_rule(integer()) -> ca:rule().
make_rule(N) when N > 0, N < 256 ->
    Unpadded = hd(io_lib:format("~.2B", [N])),
    Bstring = string:right(Unpadded, 8, $0),
    lists:zipwith(fun(Digit, Rule) ->
                          {Int, []} = string:to_integer([Digit]),
                          case Int of
                              error -> error(bad_rule);
                              _ -> {Rule, Int}
                          end
                  end, Bstring, ?RULES).

-spec make_state(integer()) -> ca:state().
make_state(N) ->
    lists:duplicate(N div 2, 0) ++ [1] ++ lists:duplicate(N div 2, 0).

-spec random_state(integer()) -> ca:state().
random_state(N) ->
    random:seed(os:timestamp()),
    [random:uniform(2) - 1 || _ <- lists:seq(1, N)].
