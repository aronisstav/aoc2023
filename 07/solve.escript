#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/7

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
  read_many("~s ~d").

read(Pat) ->
  io:fread("", Pat).

read_many(Pat) ->
  read_many(Pat, []).

read_many(Pat, Acc) ->
  case read(Pat) of
    {ok, R} -> read_many(Pat, [R|Acc]);
    _ -> lists:reverse(Acc)
  end.

solve_first(Input) ->
  Tagged = [tag(H) || H <- Input],
  score(Tagged).

score(Tagged) ->
  Sorted = lists:sort(Tagged),
  Fold = fun({_, V}, {I, Acc}) -> {I + 1, Acc + V * I} end,
  {_, Acc} = lists:foldl(Fold, {1, 0}, Sorted),
  Acc.

tag([Hand, Bet]) ->
  Fold =
    fun(C, [{V, D}|R] = A) ->
        case C == D of
          true -> [{V + 1, D}|R];
          false -> [{1, C}|A]
        end
    end,
  [_|Vs] = lists:sort(lists:foldl(Fold, [{0, $X}], lists:sort(Hand))),
  Rank = rank(lists:reverse(Vs)),
  {{Rank, [map(H) || H <- Hand], Hand}, Bet}.

rank(Vs) ->
  case Vs of
    [{1, _}|_] -> 1;
    [{2, _},{2, _}|_] -> 3;
    [{2, _}|_] -> 2;
    [{3, _},{2, _}|_] -> 5;
    [{3, _}|_] -> 4;
    [{4, _}|_] -> 6;
    [{5, _}|_] -> 7
  end.

map($2) -> 2;
map($3) -> 3;
map($4) -> 4;
map($5) -> 5;
map($6) -> 6;
map($7) -> 7;
map($8) -> 8;
map($9) -> 9;
map($T) -> 10;
map($J) -> 11;
map($Q) -> 12;
map($K) -> 13;
map($A) -> 14.

solve_second(Input) ->
  Tagged = [tag2(H) || H <- Input],
  score(Tagged).

tag2([Hand, Bet]) ->
  Fold =
    fun(C, [{V, D}|R] = A) ->
        case C == D of
          true -> [{V + 1, D}|R];
          false -> [{1, C}|A]
        end
    end,
  [_|Vs] = lists:sort(lists:foldl(Fold, [{0, $X}], lists:sort(Hand))),
  {Js, Rest} =
    case lists:keytake($J, 2, lists:reverse(Vs)) of
      false -> {0, lists:reverse(Vs)};
      {value, {JV, _}, NR} -> {JV, NR}
    end,
  Fixed = fix(Js, Rest),
  Rank = rank(Fixed),
  {{Rank, [map2(H) || H <- Hand], Hand}, Bet}.

fix(N, [{C, V}|Rest]) -> [{C + N, V}|Rest];
fix(5, _) -> [{5, $K}].

map2($J) -> 1;
map2($2) -> 2;
map2($3) -> 3;
map2($4) -> 4;
map2($5) -> 5;
map2($6) -> 6;
map2($7) -> 7;
map2($8) -> 8;
map2($9) -> 9;
map2($T) -> 10;
map2($Q) -> 12;
map2($K) -> 13;
map2($A) -> 14.
