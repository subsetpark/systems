-module(tags).
-export([process_state/2, make_state/2, make_system/2]).

-type tag_system() :: {drop_number(), alphabet(), [rule()]}.
-type drop_number() :: non_neg_integer().
-type alphabet() :: {symbol()}.
-type rule() :: {symbol(), state()}.
-type symbol() :: atom().
-type state() :: [symbol()].

-spec process_state(state(), tag_system()) -> state().
process_state(['H'|_]=State, _) ->
    State;
process_state([H|_]=State, {Drop, _, Rules}=System) ->
    R = ca:apply_rules(H, Rules),
    process_state(lists:nthtail(Drop, State) ++ R, System).

-spec make_state(non_neg_integer(), tag_system()) -> state().
make_state(Size, {_, Alphabet, _}) ->
    N = tuple_size(Alphabet),
    [element(random:uniform(N), Alphabet) || _ <- lists:seq(1, Size)].

-spec make_system(drop_number(), [rule()]) -> tag_system().
make_system(Drop, Rules) ->
    Alphabet = get_alphabet(Rules),
    {Drop, Alphabet, Rules}.

-spec get_alphabet([rule()]) -> alphabet().
get_alphabet(Rules) ->
    get_alphabet(Rules, []).
get_alphabet([], Out) ->
    list_to_tuple(lists:reverse(['H'|Out]));
get_alphabet([{Symbol, _}|Rules], Out) ->
    get_alphabet(Rules, [Symbol|Out]).

