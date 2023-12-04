#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/3

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
  {S, _} = sum(1, 1, MX, MY, {0, #{}}, Map),
  S.

sum( _, MY,  _, MY, Acc,   _) -> Acc;
sum(MX,  Y, MX, MY, Acc, Map) ->
  sum(1, Y + 1, MX, MY, Acc, Map);
sum( X,  Y, MX, MY, {Acc, GM}, Map) ->
  {Inc, NGM} = inc(X, Y, GM, Map),
  sum(X + 1, Y, MX, MY, {Acc + Inc, NGM}, Map).

inc(X, Y, GM, Map) ->
  case starts_number(X, Y, Map) of
    {ok, N} ->
      case near_symbol(X, Y, Map) of
        {ok, {XX, YY, C}} ->
          NGM =
            case C of
              $* ->
                Old = maps:get({XX,YY}, GM, []),
                GM#{{XX,YY} => [N|Old]};
              _ -> GM
            end,
          {N, NGM};
        false -> {0, GM}
      end;
    false -> {0, GM}
  end.

starts_number(X, Y, Map) ->
  {ok, C} = lookup(X, Y, Map),
  case is_num(C) of
    {ok, N} ->
      case lookup(X - 1, Y, Map) of
        {ok, D} ->
          case is_num(D) of
            {ok, _} -> false;
            false ->
              {ok, follow_num(N, X + 1, Y, Map)}
          end;
        false ->
          {ok, follow_num(N, X + 1, Y, Map)}
      end;
    false -> false
  end.

is_num(C) when C >= $0, C =< $9 -> {ok, C - $0};
is_num(_) -> false.

lookup(X, Y, Map) ->
  case Map of
    #{{X, Y} := C} -> {ok, C};
    _ -> false
  end.

follow_num(N, X, Y, Map) ->
  case lookup(X, Y, Map) of
    {ok, C} ->
      case is_num(C) of
        {ok, M} ->
          follow_num(N*10 + M, X + 1, Y, Map);
        false -> N
      end;
    false -> N
  end.

near_symbol(X, Y, Map) ->
  Ns =
    [{X + XX, Y + YY} || XX <- [-1, 0 , 1], YY <- [-1, 0, 1]],
  case any_symbol(Ns, Map) of
    {ok, _} = Ok -> Ok;
    false ->
      case follow_num(1, X + 1, Y, Map) > 1 of
        true -> near_symbol(X + 1, Y, Map);
        false -> false
      end
  end.

any_symbol([], _) ->
  false;
any_symbol([{X,Y}|R], Map) ->
  IsSymbol =
    case lookup(X, Y, Map) of
      false -> false;
      {ok, C} ->
        case C of
          $. -> false;
          $\n -> false;
          D when D >= $0, D =< $9 -> false;
          Q -> {ok, {X, Y, Q}}
        end
    end,
  case IsSymbol of
    false -> any_symbol(R, Map);
    True -> True
  end.

solve_second({MY, #{mx := MX} = Map}) ->
  {_, GM} = sum(1, 1, MX, MY, {0, #{}}, Map),
  Fold =
    fun(_, V, Acc) ->
        case V of
          [N, M] -> N * M + Acc;
          _ -> Acc
        end
    end,
  maps:fold(Fold, 0, GM).
