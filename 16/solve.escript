#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/16

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
              NMap =
                case C =/= $. of
                  true -> Map#{{I, J} => C};
                  false -> Map
                end,
              {I + 1, NMap#{mx => max(MX, I)}}
          end,
        {_, M} = lists:foldl(Fold2, {1, Acc}, Line),
        {J + 1, M}
    end,
  lists:foldl(Fold1, {1, #{mx => 0}}, Lines).

solve_first(Input) ->
  flow(Input, {1, 1, r}).

flow(Input, Init) ->
  NMap = flow(queue:from_list([Init]), Input, #{}),
  Fold = fun({X, Y, _}, _, Acc) -> Acc#{{X,Y} => true} end,
  maps:size(maps:fold(Fold, #{}, NMap)).

flow(Queue, {MY, #{mx := MX} = M} = Map, Acc) ->
  {Out, NQ} = queue:out(Queue),
  case Out of
    empty -> Acc;
    {value, {X, Y, D}} ->
      NAcc = Acc#{{X,Y, D} => true},
      C = maps:get({X,Y}, M, $.),
      CNew =
        case {C, D} of
          {$-, D} when D=:=u; D=:=d ->
            [pass($/,X,Y,D),
             pass($\\,X,Y,D)];
          {$|, D} when D=:=l; D=:=r ->
            [pass($/,X,Y,D),
             pass($\\,X,Y,D)];
          {C, D} -> [pass(C,X,Y,D)]
        end,
      New =
        [N || {XX, YY, _} = N <- CNew,
              XX < MX, YY < MY,
              XX > 0, YY > 0,
              maps:get(N, Acc, x) =:= x],
      FQ = lists:foldl(fun queue:in/2, NQ, New),
      flow(FQ, Map, NAcc)
  end.

pass($/, X, Y, u) -> {X + 1,     Y, r};
pass($/, X, Y, l) -> {    X, Y + 1, d};
pass($/, X, Y, r) -> {    X, Y - 1, u};
pass($/, X, Y, d) -> {X - 1,     Y, l};
pass($\\, X, Y, D) -> pass($/, X, Y, op(D));
pass(_, X, Y, u) -> {    X, Y - 1, u};
pass(_, X, Y, d) -> {    X, Y + 1, d};
pass(_, X, Y, l) -> {X - 1,     Y, l};
pass(_, X, Y, r) -> {X + 1,     Y, r}.

op(u) -> d;
op(d) -> u;
op(l) -> r;
op(r) -> l.

solve_second({MY, #{mx := MX}} = Input) ->
  Fold = fun(C, Max) -> max(Max, flow(Input, C)) end,
  TR = [{X, 1, d} || X <- lists:seq(1, MX - 1)],
  BR = [{X, MY - 1, u} || X <- lists:seq(1, MX - 1)],
  LC = [{1, Y, r} || Y <- lists:seq(1, MY - 1)],
  RC = [{MX - 1, Y, l} || Y <- lists:seq(1, MY - 1)],
  lists:foldl(Fold, 0, TR ++ BR ++ LC ++ RC).

%% print({MY, #{mx := MX} = Map}) ->
%%   io:format("~n"),
%%   FoldY =
%%     fun(Y) ->
%%         FoldX =
%%           fun(X) ->
%%               C = maps:get({X,Y}, Map, $ ),
%%               io:format("~c", [C])
%%           end,
%%         lists:foreach(FoldX, lists:seq(1, MX - 1)),
%%         io:format("~n")
%%     end,
%%   lists:foreach(FoldY, lists:seq(1, MY - 1)).
