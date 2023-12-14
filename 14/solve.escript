#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/14

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

solve_first(Input) ->
  North = move(Input, {0, -1}, {0, 1}),
  weight(North).

move({MY, #{mx := MX} = Map}, {DX, DY} = Move, Bound) ->
  Xs = lists:seq(1, MX - 1),
  Ys = lists:seq(1, MY - 1),
  ScanX =
    case DX =:= 1 of
      true -> lists:reverse(Xs);
      false -> Xs
    end,
  ScanY =
    case DY =:= 1 of
      true -> lists:reverse(Ys);
      false -> Ys
    end,
  FoldY =
    fun(Y, YMap) ->
        FoldX = fun(X, XMap) ->  move({X,Y}, XMap, Move, Bound) end,
        lists:foldl(FoldX, YMap, ScanX)
    end,
  {MY, lists:foldl(FoldY, Map, ScanY)}.

move({X, Y}, Map, _, {A, B}) when X =:= A; Y =:= B -> Map;
move({X, Y}, Map, {A, B} = Move, Bound) ->
  TPos = {X, Y},
  NPos = {X + A, Y + B},
  #{ TPos := T
   , NPos := N
   } = Map,
  case {T, N} == {$O, $.} of
    true -> move(NPos, Map#{TPos => $., NPos => $O}, Move, Bound);
    false -> Map
  end.

weight({MY, #{mx := MX} = Map}) ->
  FoldY =
    fun(Y, YAcc) ->
        FoldX =
          fun(X, XAcc) ->
              #{{X,Y} := T} = Map,
              case T == $O of
                true -> XAcc + MY - Y;
                false -> XAcc
              end
          end,
        lists:foldl(FoldX, YAcc, lists:seq(1, MX - 1))
    end,
  lists:foldl(FoldY, 0, lists:seq(1, MY - 1)).

solve_second(Input) ->
  Spun = spin(Input, #{}, 0, false),
  %% print(Spun),
  weight(Spun).

spin(Map, _, C, C) -> Map;
spin({MY, #{mx := MX}} = I, FT, C, Max) ->
  case FT of
    #{I := {V, Nxt}} ->
      NMax =
        case Max =:= false of
          true ->  C + (1000000000 - V) rem (C - V);
          false -> Max
        end,
      spin(Nxt, FT, C + 1, NMax);
    _ ->
      N = move(I, { 0, -1}, { 0,  1}),
      W = move(N, {-1,  0}, { 1,  0}),
      S = move(W, { 0,  1}, { 0, MY - 1}),
      E = move(S, { 1,  0}, {MX - 1,  0}),
      spin(E, FT#{I => {C, E}}, C + 1, Max)
  end.

%% print({MY, #{mx := MX} = Map}) ->
%%   io:format("~n"),
%%   FoldY =
%%     fun(Y) ->
%%         FoldX =
%%           fun(X) ->
%%               #{{X, Y} := C} = Map,
%%               io:format("~c", [C])
%%           end,
%%         lists:foreach(FoldX, lists:seq(1, MX - 1)),
%%         io:format("~n")
%%     end,
%%   lists:foreach(FoldY, lists:seq(1, MY - 1)).
