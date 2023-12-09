#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/9

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
    Res ->
      [_|R] = lists:reverse(Res),
      Ns =
        [list_to_integer(N) || N <- string:split(lists:reverse(R), " ", all)],
      read_lines([Ns|Acc])
  end.

solve_first(Input) ->
  Fold = fun(S, Acc) -> Acc + pred(S) end,
  lists:foldl(Fold, 0, Input).

pred([H|T] = S) ->
  case lists:all(fun(X) -> X =:= 0 end, S) of
    true -> 0;
    false ->
      Fold = fun(V, {L, Acc}) -> {V, [V - L|Acc]} end,
      {L, DR} = lists:foldl(Fold, {H, []}, T),
      L + pred(lists:reverse(DR))
  end.

solve_second(Input) ->
  Fold = fun(S, Acc) -> Acc + prev(S) end,
  lists:foldl(Fold, 0, Input).

prev([H|T] = S) ->
  case lists:all(fun(X) -> X =:= 0 end, S) of
    true -> 0;
    false ->
      Fold = fun(V, {L, Acc}) -> {V, [V - L|Acc]} end,
      {_, DR} = lists:foldl(Fold, {H, []}, T),
      H - prev(lists:reverse(DR))
  end.
