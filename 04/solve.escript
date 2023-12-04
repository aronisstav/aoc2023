#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/4

-mode(compile).

main(Args) ->
  Input = read_lines(),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~p~n", [Ans]).

read_lines() -> read_lines([]).

read_lines(Acc) ->
  case io:get_line("") of
    eof -> lists:reverse(Acc);
    ResX ->
      [_|T] = lists:reverse(ResX),
      Res = lists:reverse(T),
      read_lines([parse(Res)|Acc])
  end.

parse(Line) ->
  {ok, [N], Rest} = io_lib:fread("Card ~d: ", Line),
  Split = string:split(Rest, " ", all),
  Fold =
    fun(S, {P, W, M}) ->
        case {P, S} of
          {_, ""} -> {P, W, M};
          {false, "|"} -> {true, W, M};
          {false, _} -> {P, [list_to_integer(S)|W], M};
          {true, _} -> {P, W, [list_to_integer(S)|M]}
        end
    end,
  {_, W, M} = lists:foldl(Fold, {false, [], []}, Split),
  {N, {W, M}}.

solve_first(Input) ->
  Fold =
    fun({_, {W, M}}, Acc) ->
        Inc =
          case score(W, M) of
            0 -> 0;
            N -> math:pow(2, N - 1)
          end,
        Acc + Inc
    end,
  lists:foldl(Fold, 0, Input).

score(W, M) ->
  WS = sets:from_list(W),
  MS = sets:from_list(M),
  PS = sets:intersection(WS, MS),
  sets:size(PS).

solve_second(Input) ->
  Pile = maps:from_list(Input),
  reduce(1, Pile, #{}, 0).

reduce(N, Pile, Cs, S) ->
  case Pile of
    #{N := {W, M}} ->
      Win = score(W, M),
      Inc = maps:get(N, Cs, 1),
      NCs = add(Win, N + 1, Inc, Cs),
      reduce(N + 1, Pile, NCs, S + Inc);
    _ -> S
  end.

add(0, _,   _, Cs) -> Cs;
add(N, I, Inc, Cs) ->
  IC = maps:get(I, Cs, 1),
  add(N - 1, I + 1, Inc, Cs#{I => IC + Inc}).
