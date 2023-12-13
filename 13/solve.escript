#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/13

-mode(compile).

main(Args) ->
  Input = read_lines(),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~p~n", [Ans]).

read_lines() ->
  to_maps(read_lines([])).

read_lines(Acc) ->
  case io:get_line("") of
    eof -> lists:reverse(["\n"|Acc]);
    Res -> read_lines([Res|Acc])
  end.

to_maps(Lines) ->
  Fold1 =
    fun("\n", {J, Map, Maps}) ->
        {1, #{mx => 0}, [{J, Map}|Maps]};
       (Line, {J, Acc, Maps}) ->
        Fold2 =
          fun(C, {I, #{mx := MX} = Map}) ->
              {I + 1, Map#{{I, J} => C, mx => max(MX, I)}}
          end,
        {_, M} = lists:foldl(Fold2, {1, Acc}, Line),
        {J + 1, M, Maps}
    end,
  {_, _, Maps} = lists:foldl(Fold1, {1, #{mx => 0}, []}, Lines),
  lists:reverse(Maps).

solve_first(Maps) ->
  lists:sum([score(M, 0) || M <- Maps]).

score(Map, E) ->
  case can_vert(Map, E) of
    false -> 100 * can_hor(Map, E);
    N -> N
  end.

can_vert({MY, #{mx := MX} = Map}, E) ->
  can_vert(1, MX - 1, MY, Map, fun(A) -> A end, E).

can_vert(MX, MX,  _,   _,   _, _) -> false;
can_vert( I, MX, MY, Map, Key, E) ->
  Ds = lists:seq(0, min(I - 1, MX - I - 1)),
  case can_vert1(I, Ds, MY, Map, Key, E) of
    true -> I;
    false -> can_vert(I + 1, MX, MY, Map, Key, E)
  end.

can_vert1(_,  _, _, _, _, E) when E < 0 -> false;
can_vert1(_, [], _, _, _, 0) -> true;
can_vert1(_, [], _, _, _, _) -> false;
can_vert1(I, [D|Ds], MY, Map, Key, E) ->
  Ys = lists:seq(1, MY - 1),
  Fold =
    fun(Y, Acc) ->
        K1 = Key({I - D, Y}),
        K2 = Key({I + D + 1, Y}),
        #{ K1 := C1
         , K2 := C2
         } = Map,
        case C1 =:= C2 of
          true -> Acc;
          false -> Acc - 1
        end
    end,
  NE = lists:foldl(Fold, E, Ys),
  can_vert1(I, Ds, MY, Map, Key, NE).

can_hor({MY, #{mx := MX} = Map}, E) ->
  can_vert(1, MY - 1, MX, Map, fun({X, Y}) -> {Y, X} end, E).

solve_second(Maps) ->
  lists:sum([score(M, 1) || M <- Maps]).
