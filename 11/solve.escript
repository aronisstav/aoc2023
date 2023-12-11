#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/11

-mode(compile).

main(Args) ->
  Input = read_lines(),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~w~n", [Ans]).

read_lines() ->
  to_map(read_lines([])).

read_lines(Acc) ->
  case io:get_line("") of
    eof -> lists:reverse(Acc);
    Res -> read_lines([Res|Acc])
  end.

to_map(Lines) ->
  Fold1 =
    fun(Line, {J, Acc}) ->
        Fold2 =
          fun(C, {I, #{mx := MX} = Map}) ->
              {I + 1, Map#{{I, J} => C, mx => max(MX, I)}}
          end,
        {_, M} = lists:foldl(Fold2, {1, Acc}, Line),
        {J + 1, M}
    end,
  lists:foldl(Fold1, {1, #{mx => 0}}, Lines).

solve_first({MY, #{mx := MX} = Map}) ->
  solve(MX, MY, Map, 1).

solve(MX, MY, Map, Scale) ->
  {_, GG} = num_gals(Map),
  WR = wide_rows(MX, GG),
  WC = wide_cols(MY, GG),
  sum_dist(GG, WR, WC, Scale) div 2.

num_gals(Map) ->
  Fold =
    fun(Pos, C, {I, Acc}) ->
        case C of
          $# -> {I + 1, Acc#{I => Pos}};
          _ -> {I, Acc}
        end
    end,
  maps:fold(Fold, {1, #{}}, Map).

wide_rows(MX, GG) ->
  Fold = fun(_, {X, _}, Acc) -> Acc -- [X] end,
  maps:fold(Fold, lists:seq(1, MX - 1), GG).

wide_cols(MY, GG) ->
  Fold = fun(_, {_, Y}, Acc) -> Acc -- [Y] end,
  maps:fold(Fold, lists:seq(1, MY - 1), GG).

sum_dist(MG, WR, WC, Scale) ->
  Fold =
    fun(_, {X, Y}, Acc) ->
        Fold2 =
          fun(_, {XX, YY}, IAcc) ->
              WRA = length([I || I <- WR, I >= min(X, XX), I =< max(X, XX)]),
              WCA = length([I || I <- WC, I >= min(Y, YY), I =< max(Y, YY)]),
              abs(X - XX) + abs(Y - YY) + Scale * WRA + Scale * WCA + IAcc
          end,
        maps:fold(Fold2, Acc, MG)
    end,
  maps:fold(Fold, 0, MG).

solve_second({MY, #{mx := MX} = Map}) ->
  solve(MX, MY, Map, 999999).
