#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/17

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
              {I + 1, Map#{{I, J} => C - $0, mx => max(MX, I)}}
          end,
        {_, M} = lists:foldl(Fold2, {1, Acc}, Line),
        {J + 1, M}
    end,
  lists:foldl(Fold1, {1, #{mx => 0}}, Lines).

solve_first(Input) ->
  explore(new({0, {{1, 1}, {d, 3}}}), Input, #{}, fun check/7, fun fin/2).

new(Elem) -> gb_sets:from_list([Elem]).
smallest(Set) -> gb_sets:take_smallest(Set).
add(Elems, Set) ->
  lists:foldl(fun gb_sets:add_element/2, Set, Elems).

getHL(1, 1, _) -> 0;
getHL(X, Y, {_, Map}) ->
  #{{X, Y} := V} = Map,
  V.

explore(Open, Map, Vis, Check, Fin) ->
  {{HL, {{X, Y}, _} = Pos}, NOpen} = smallest(Open),
  CHL = getHL(X, Y, Map),
  NHL = HL + CHL,
  NVis = Vis#{Pos => NHL},
  case Fin(Pos, Map) of
    true -> NHL;
    false ->
      Nexts =
        [{NHL, N} ||
          N <- next(Pos, Map, Check),
          maps:get(N, NVis, x) =:= x],
      %%io:format("~p -> ~p~n", [Cur, Nexts]),
      FOpen = add(Nexts, NOpen),
      explore(FOpen, Map, NVis, Check, Fin)
  end.

fin({{X,Y},_}, {MY, #{mx := MX}}) ->
  {X, Y} =:= {MX - 1, MY - 1}.

next({{X, Y}, {F, M}}, {MY, #{mx := MX}}, Check) ->
  U = Check(X, Y - 1, u, F, M, -1,  0),
  D = Check(X, Y + 1, d, F, M, -1, MY),
  L = Check(X - 1, Y, l, F, M,  0, -1),
  R = Check(X + 1, Y, r, F, M, MX, -1),
  U ++ D ++ L ++ R.

check(X, _, _, _, _, X, _) -> [];
check(_, Y, _, _, _, _, Y) -> [];
check(_, _, d, u, _, _, _) -> [];
check(_, _, u, d, _, _, _) -> [];
check(_, _, r, l, _, _, _) -> [];
check(_, _, l, r, _, _, _) -> [];
check(_, _, D, D, 0, _, _) -> [];
check(X, Y, D, D, R, _, _) -> [{{X, Y}, {D, R - 1}}];
check(X, Y, D, _, _, _, _) -> [{{X, Y}, {D, 2}}].

solve_second(Input) ->
  explore(new({0, {{1, 1}, {x, 0}}}), Input, #{}, fun check1/7, fun fin1/2).

check1(X, _, _, _, _, X, _) -> [];
check1(_, Y, _, _, _, _, Y) -> [];
check1(_, _, d, u, _, _, _) -> [];
check1(_, _, u, d, _, _, _) -> [];
check1(_, _, r, l, _, _, _) -> [];
check1(_, _, l, r, _, _, _) -> [];
check1(_, _, D, D, 0, _, _) -> [];
check1(X, Y, D, D, R, _, _) -> [{{X, Y}, {D, R - 1}}];
check1(_, _, _, _, R, _, _) when R > 6 -> [];
check1(X, Y, D, _, _, _, _) -> [{{X, Y}, {D, 9}}].

fin1({{X,Y},{_,M}}, {MY, #{mx := MX}}) ->
  {X, Y} =:= {MX - 1, MY - 1} andalso
    M < 7.
