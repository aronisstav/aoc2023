#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/10

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
              SMap =
                case C =:= $S of
                  true -> Map#{s => {I, J}};
                  false -> Map
                end,
              {I + 1, SMap#{{I, J} => C, mx => max(MX, I)}}
          end,
        {_, M} = lists:foldl(Fold2, {1, Acc}, Line),
        {J + 1, M}
    end,
  lists:foldl(Fold1, {1, #{mx => 0}}, Lines).

solve_first({_MY, #{s := S} = Map}) ->
  M2 = follow(S, x, 0, Map, #{}),
  follow_back(S, 0, M2).

follow(Pos, Prev, N, Map, Acc) ->
  case next(Pos, Prev, Map) of
    {done, Prev} -> Acc#{Pos => {N, Prev}};
    Next ->
      follow(Next, Pos, N + 1, Map, Acc#{Pos => {N, Prev}})
  end.

next({X, Y} = Pos, Prev, Map) ->
  #{Pos := C
   } = Map,
  case {C, Prev} of
    {$S, x} ->
      U = get({X, Y - 1}, Map),
      case connects_down(U) of
        true -> put(s, up), {X, Y-1};
        false ->
          D = get({X, Y + 1}, Map),
          case connects_up(D) of
            true -> put(s, down), {X, Y+1};
            false -> put(s, $-), {X+1,Y}
          end
      end;
    {$S, _} ->
      %% It is too tedious to recover what kind of piece S is, needed
      %% for part 2. Use the proc dic in two parts (either it is a -
      %% or we know where we went to initially, and where we came back
      %% from. Answer produced by manual inspection, code added later.
      case get(s) of
        $- -> ok;
        down ->
          case Prev of
            {X, _} -> put(s, $|);
            {L, _} when L < X -> put(s, $7);
            _ ->  put(s, $F)
          end;
        up ->
          case Prev of
            {L, _} when L < X -> put(s, $J);
            _ ->  put(s, $L)
          end
      end,
      {done, Prev};
    {$|, {_, L}} when L < Y -> {X,Y+1};
    {$|,      _} -> {X,Y-1};
    {$-, {L, _}} when L < X -> {X+1,Y};
    {$-,      _} -> {X-1, Y};
    {$L, {_, Y}} -> {X,Y-1};
    {$L,      _} -> {X+1,Y};
    {$J, {_, Y}} -> {X,Y-1};
    {$J,      _} -> {X-1,Y};
    {$7, {_, Y}} -> {X,Y+1};
    {$7,      _} -> {X-1,Y};
    {$F, {_, Y}} -> {X,Y+1};
    {$F,      _} -> {X+1, Y}
  end.

follow_back(Pos, N, Map) ->
  #{Pos := {M, Prev}
   } = Map,
  case M =< N of
    true -> N;
    false ->
      follow_back(Prev, N + 1, Map)
  end.


get(Pos, Map) ->
  maps:get(Pos, Map, x).

connects_up(C) ->
  case C of
    $| -> true;
    $L -> true;
    $J -> true;
    _ -> false
  end.

connects_down(C) ->
  case C of
    $| -> true;
    $7 -> true;
    $F -> true;
    _ -> false
  end.

solve_second({MY, #{mx := MX, s := S} = Map}) ->
  Loop = follow(S, x, 0, Map, #{}),
  count_in(Map, Loop, MX, MY).

%% Keep a parity count about how many times we have passed pipe
%% belonging to the loop seems to work.
count_in(Map, Loop, MX, MY) ->
  FoldY =
    fun(Y, AccY) ->
        FoldX =
          fun(X, {PD, PU, Acc}) ->
              case maps:find({X,Y}, Loop) of
                {ok, _} ->
                  #{{X,Y} := C} = Map,
                  {NPD, NPU} =
                    case C of
                      $S -> parity_map(get(s), PD, PU);
                      _ -> parity_map(C, PD, PU)
                    end,
                  {NPD, NPU, Acc};
                _ ->
                  NAcc =
                    case PD =:= 1 orelse PU =:= 1 of
                      true -> Acc + 1;
                      false -> Acc
                    end,
                  {PD, PU, NAcc}
              end
          end,
        {0, 0, NAcc} = lists:foldl(FoldX, {0,0,AccY}, lists:seq(1,MX -1)),
        NAcc
    end,
  lists:foldl(FoldY, 0, lists:seq(1,MY -1)).


parity_map(C, PD, PU) ->
  case C of
    $| -> {1 - PD, 1 - PU};
    $- -> {    PD,     PU};
    $L -> {    PD, 1 - PU};
    $J -> {    PD, 1 - PU};
    $7 -> {1 - PD,     PU};
    $F -> {1 - PD,     PU}
  end.
