#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/15

-mode(compile).

main(Args) ->
  Input = read_input(),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~p~n", [Ans]).

read_input() ->
  {ok, [S]} = io:fread("", "~s"),
  string:split(S, ",", all).

solve_first(Input) ->
  lists:sum([hash(S) || S <- Input]).

hash(String) ->
  Fold = fun(C, Acc) -> ((Acc + C) * 17) rem 256 end,
  lists:foldl(Fold, 0, String).

solve_second(Input) ->
  Place = place(Input),
  calc(Place).

place(Input) ->
  Fold =
    fun(S, Map) ->
        [Label|T] = string:tokens(S, "=-"),
        Pos = hash(Label),
        Box = maps:get(Pos, Map, []),
        case T of
          [] ->
            Map#{Pos => lists:keydelete(Label, 1, Box)};
          [VS] ->
            V = list_to_integer(VS),
            NBox = replace(Label, V, Box),
            Map#{Pos => NBox}
        end
    end,
  lists:foldl(Fold, #{}, Input).

replace(Label, V, []) ->
  [{Label, V}];
replace(Label, V, [{Label, _}|T]) ->
  [{Label, V}|T];
replace(Label, V, [H|T]) ->
  [H|replace(Label, V, T)].

calc(Map) ->
  Fold =
    fun(I, Acc) ->
        Box = maps:get(I, Map, []),
        {_, Vs} = lists:unzip(Box),
        Sum = lists:sum([P*V || {P, V} <- lists:enumerate(Vs)]),
        Acc + (I + 1) * Sum
    end,
  lists:foldl(Fold, 0, lists:seq(0, 255)).
