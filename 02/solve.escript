#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/2

-mode(compile).

main(Args) ->
  Input = read_lines(),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~w~n", [Ans]).

read_lines() -> read_lines([]).

read_lines(Acc) ->
  case io:get_line("") of
    eof -> lists:reverse(Acc);
    Res -> read_lines([parse(Res)|Acc])
  end.

parse(Line) ->
  {ok, [Id], Rest} = io_lib:fread("Game ~d: ", Line),
  Rounds = string:split(Rest, "; ", all),
  F1 =
    fun(Round, Acc1) ->
        Items = string:split(Round, ", ", all),
        F2 =
          fun(Item, Acc2) ->
              {ok, [N, C], _} = io_lib:fread("~d ~s", Item),
              [{N, C}|Acc2]
          end,
        [lists:foldl(F2, [], Items)|Acc1]
    end,
  {Id, lists:foldl(F1, [], Rounds)}.

solve_first(Input) ->
  lists:sum([Id || {Id, Rounds} <- Input, possible(Rounds)]).

possible(Rounds) ->
  F1 =
    fun(Items) ->
        F2 =
          fun({N, Item}) ->
              case Item of
                "red"   -> N =< 12;
                "green" -> N =< 13;
                "blue"  -> N =< 14
              end
          end,
        lists:all(F2, Items)
    end,
  lists:all(F1, Rounds).

solve_second(Input) ->
  lists:sum([power(Rounds) || {_, Rounds} <- Input]).

power(Rounds) ->
  F1 =
    fun(Items, Acc) ->
        F2 =
          fun({N, Item}, {R, G, B}) ->
              case Item of
                "red"   -> {max(N, R), G, B};
                "green" -> {R, max(N, G), B};
                "blue"  -> {R, G, max(N, B)}
              end
          end,
        lists:foldl(F2, Acc, Items)
    end,
  {R, G, B} = lists:foldl(F1, {0, 0, 0}, Rounds),
  R * G * B.
