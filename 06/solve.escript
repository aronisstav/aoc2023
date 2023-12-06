#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/6

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
  read("Time: "),
  Tokens = lists:append(read_many("~s")),
  Fold =
    fun(Token, {Mode, Ts, Ds}) ->
        case Token of
          "Distance:" -> {d, Ts, Ds};
          NS ->
            N = list_to_integer(NS),
            case Mode of
              t -> {Mode, [N|Ts], Ds};
              d -> {Mode, Ts, [N|Ds]}
            end
        end
    end,
  {_, Ts, Ds} = lists:foldl(Fold, {t, [], []}, Tokens),
  lists:reverse(lists:zip(Ts, Ds)).

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
  Fold = fun(R, Acc) -> Acc * ways(R) end,
  lists:foldl(Fold, 1, Input).

ways({T, D}) ->
  ways(1, T, D, 0).

ways(T, T, _, Acc) -> Acc;
ways(I, T, D, Acc) ->
  NAcc =
    case I * (T - I) > D of
      true -> Acc + 1;
      false -> Acc
    end,
  ways(I + 1, T, D, NAcc).

solve_second(Input) ->
  {TD, DD} = lists:unzip(Input),
  Fun =
    fun(X) -> list_to_integer(lists:append([integer_to_list(I) || I <- X])) end,
  T = Fun(TD),
  D = Fun(DD),
  M = T div 2,
  true = M * (T - M) > D,
  L = find_flip(1, M, T, D, up),
  H = find_flip(M, T, T, D, down),
  H - L.

find_flip(L, H, T, D, Dir) ->
  M = (L + H) div 2,
  case {r(M, T, D), r(M + 1, T, D), Dir} of
    {true, true, up} ->
      find_flip(L, M - 1, T, D, Dir);
    {true, true, down} ->
      find_flip(M + 1, H, T, D, Dir);
    {false, false, up} ->
      find_flip(M + 1, H, T, D, Dir);
    {false, false, down} ->
      find_flip(L, M - 1, T, D, Dir);
    {_, _, _} -> M
  end.

r(I, T, D) ->
  I * (T - I) > D.
