#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/8

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
  {ok, [LR]} = read("~s"),
  Map = read_many("~3s = (~3s, ~3s)"),
  M = maps:from_list([{H, {L, R}} || [H, L, R] <- Map]),
  {LR, M}.

read(Pat) ->
  io:fread("", Pat).

read_many(Pat) ->
  read_many(Pat, []).

read_many(Pat, Acc) ->
  case read(Pat) of
    {ok, R} -> read_many(Pat, [R|Acc]);
    _ -> lists:reverse(Acc)
  end.

solve_first({LR, Map}) ->
  flow("AAA", LR, LR, 0, Map).

flow("ZZZ", _, _, N, _) -> N;
flow(Loc, [], LR, N, Map) -> flow(Loc, LR, LR, N, Map);
flow(Loc, [C|Cs], LR, N, Map) ->
  NLoc = send(Loc, C, Map),
  flow(NLoc, Cs, LR, N + 1, Map).

send(Loc, C, Map) ->
  #{Loc := {L, R}
   } = Map,
  case C of
    $R -> R;
    $L -> L
  end.

solve_second({LR, Map}) ->
  Starts = starts(Map),
  %% Observe that checking every start simultaneously is slow.
  %% Observe that checking each start individually yields goals in at most
  %%  50000 steps
  %% Observe that if one continues to walk after a goal, they are in a perfect
  %%  cycle, so they will all sync at their Least Common Multiple...
  lists:foldl(fun lcm/2, 1, [flow2(S, LR, LR, 0, Map) || S <- Starts]).

starts(Map) ->
  Fold =
    fun(K, _, Acc) ->
        case K of
          [_, _, $A] -> [K|Acc];
          _ -> Acc
        end
    end,
  maps:fold(Fold, [], Map).

flow2([_,_,$Z], _, _, N, _) -> N;
flow2(Loc, [], LR, N, Map) -> flow2(Loc, LR, LR, N, Map);
flow2(Loc, [C|Cs], LR, N, Map) ->
  NLoc = send(Loc, C, Map),
  flow2(NLoc, Cs, LR, N + 1, Map).

%% From https://www.programming-idioms.org/idiom/75/compute-lcm/2484/erlang
gcd(A,B) when A == 0; B == 0 -> 0;
gcd(A,B) when A == B -> A;
gcd(A,B) when A > B -> gcd(A-B, B);
gcd(A,B) -> gcd(A, B-A).

lcm(A,B) -> (A*B) div gcd(A, B).
