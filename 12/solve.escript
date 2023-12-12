#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/12

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
  [{L, split(D)} || [L, D] <- read_many("~s ~s")].

read_many(Pat) ->
  read_many(Pat, []).

read_many(Pat, Acc) ->
  case read(Pat) of
    {ok, R} -> read_many(Pat, [R|Acc]);
    _ -> lists:reverse(Acc)
  end.

read(Pat) ->
  io:fread("", Pat).

split(D) ->
  [list_to_integer(N) || N <- string:split(D, ",", all)].

%% For first case, brute force should be fine, as there are at most 16
%% ? in each input.
solve_first(Input) ->
  Fold = fun(I, Acc) -> Acc + count(I) end,
  lists:foldl(Fold, 0, Input).

count({S, N}) ->
  length([C || C <- generate(S), satisfy(C, N)]).

generate(S) ->
  generate(S, [[]]).

generate([], Acc) -> [lists:reverse(I) || I <- Acc];
generate([C|R], Acc) ->
  NAcc =
    case C of
      $? -> [[$.|T] || T <- Acc] ++ [[$#|T] || T <- Acc];
      _ -> [[C|T] || T <- Acc]
    end,
  generate(R, NAcc).

satisfy(S, N) ->
  satisfy(S, N, 0).

satisfy([], [], _) -> true;
satisfy([C|R], [], _) ->
  case C of
    $. -> satisfy(R, [], 0);
    $# -> false
  end;
satisfy([], [N], N) -> true;
satisfy([C|R], [N|T] = Ns, M) when M =< N ->
  case {C, M} of
    {$., 0} -> satisfy(R, Ns, M);
    {$., _} ->
      case N =:= M of
        true ->  satisfy(R, T, 0);
        false -> false
      end;
    {$#, _} -> satisfy(R, Ns, M + 1)
  end;
satisfy(_, _, _) -> false.

%% For part two, keep track of how many ways we can reach a suffix
solve_second(Input) ->
  Fold = fun(I, Acc) -> Acc + count_exp(I) end,
  lists:foldl(Fold, 0, Input).

count_exp(Input) ->
  E = expand(Input, 5),
  explore(queue:from_list([E]), #{E => 1}).

expand({SS, NN}, M) ->
  S = string:join([SS || _ <- lists:seq(1, M)], "?"),
  N = lists:append([NN || _ <- lists:seq(1, M)]),
  {S, N, 0}.

explore(Queue, States) ->
  {Out, NQ} = queue:out(Queue),
  case Out of
    empty -> maps:get(final, States, 0);
    {value, State} ->
      #{State := M
       } = States,
      Viables = viables(State),
      %%io:format("~p~n", [{State, M, Viables}]),
      Fold =
        fun(Via, {QAcc, SAcc}) ->
            Q = maps:get(Via, SAcc, 0),
            NSAcc = SAcc#{Via => Q + M},
            NQAcc =
              case Via =/= final andalso Q =:= 0 of
                true -> queue:in(Via, QAcc);
                false  -> QAcc
              end,
            {NQAcc, NSAcc}
        end,
      {NQueue, NStates} = lists:foldl(Fold, {NQ, States}, Viables),
      explore(NQueue, NStates)
  end.

viables({[],  [], 0}) -> [final];
viables({[], [N], N}) -> [final];
viables({[],  _,  _}) -> [];
viables({[$.|Cs], [], 0}) -> [{Cs, [], 0}];
viables({[$.|Cs], [N|Ns] = M, R}) ->
  case {R, N} of
    {0, _} -> [{Cs, M, R}];
    {I, I} -> [{Cs, Ns, 0}];
    _ -> []
  end;
viables({[$#| _], [], _}) -> [];
viables({[$#|Cs], [N|_] = M, R}) ->
  NR = R + 1,
  case NR =< N of
    true -> [{Cs, M, NR}];
    false -> []
  end;
viables({[$?|Cs], M, R}) ->
  viables({[$.|Cs], M, R}) ++ viables({[$#|Cs], M, R}).
