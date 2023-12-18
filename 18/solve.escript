#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/18

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
  Map = read_many("~c  ~d  (#~6s)"),
  [list_to_tuple(L) || L <- Map].

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
  Border = loop(Input),
  Fill = fill(Border),
  maps:size(Fill).

loop(Input) ->
  Fold =
    fun({D, L, _}, {Pos, Acc}) ->
        {End, New} = line(Pos, D, L),
        Fold1 = fun(P, A) -> A#{P => true} end,
        {End, lists:foldl(Fold1, Acc, New)}
    end,
  {_, L} = lists:foldl(Fold, {{1,1}, #{}}, Input),
  L.

line({X, Y}, D, L) ->
  [H|_] = NL =
    case D of
      "R" -> [{XX, Y} || XX <- lists:seq(X + L, X, -1)];
      "L" -> [{XX, Y} || XX <- lists:seq(X - L, X,  1)];
      "D" -> [{X, YY} || YY <- lists:seq(Y + L, Y, -1)];
      "U" -> [{X, YY} || YY <- lists:seq(Y - L, Y,  1)]
    end,
  {H, NL}.

fill(Map) ->
  Ls = mins(Map),
  Ms = maxes(Map),
  fill(queue:from_list([{2,2}]), Ls, Ms, Map).

fill(Q, Ls, Ms, Map) ->
  {V, NQ} = queue:out(Q),
  case V of
    empty -> Map;
    {value, Pos} ->
      case maps:get(Pos, Map, false) of
        true -> fill(NQ, Ls, Ms, Map);
        false ->
          NMap = Map#{Pos => true},
          Ns = [N || N <- ns(Pos, Ls, Ms), maps:get(N, Map, false) =:= false],
          FQ = lists:foldl(fun queue:in/2, NQ, Ns),
          fill(FQ, Ls, Ms, NMap)
      end
  end.

ns({X, Y}, {LX, LY}, {MX, MY}) ->
  [N || {XX, YY} = N <- [{X+1,Y}, {X-1,Y}, {X,Y+1}, {X,Y-1}],
        LX =< XX, LY =< YY,
        XX =< MX, YY =< MY].

maxes(Map) ->
  Fold = fun({X,Y}, _, {MX, MY}) -> {max(X, MX), max(Y, MY)} end,
  maps:fold(Fold, {1, 1}, Map).

mins(Map) ->
  Fold = fun({X,Y}, _, {MX, MY}) -> {min(X, MX), min(Y, MY)} end,
  maps:fold(Fold, {1, 1}, Map).

solve_second(Input) ->
  Coords = [decode(S) || {_,_,S} <- Input],
  Fold =
    fun({D, Dist}, {{X, Y}, Acc}) ->
        {NX, NY} =
          case D of
            "R" -> {X + Dist, Y};
            "D" -> {X, Y + Dist};
            "L" -> {X - Dist, Y};
            "U" -> {X, Y - Dist}
          end,
        {{NX, NY}, Acc + Dist + X * NY - Y * NX}
    end,
  {{1,1}, Sum} = lists:foldl(Fold, {{1,1}, 0}, Coords),
  Sum div 2 + 1.

decode(S) ->
  {V, DS} = lists:split(5, S),
  D =
    case DS of
      "0" -> "R";
      "1" -> "D";
      "2" -> "L";
      "3" -> "U"
    end,
  {D, list_to_integer(V, 16)}.


%% print({LX, LY}, {MX, MY}, Map) ->
%%   io:format("~n"),
%%   FoldY =
%%     fun(Y) ->
%%         FoldX =
%%           fun(X) ->
%%               C =
%%                 case maps:get({X,Y}, Map, false) of
%%                   true -> $#;
%%                   false -> $.
%%                 end,
%%               io:format("~c", [C])
%%           end,
%%         lists:foreach(FoldX, lists:seq(LX, MX)),
%%         io:format("~n")
%%     end,
%%   lists:foreach(FoldY, lists:seq(LY, MY)).

%% print(Map) ->
%%   {LX, LY} = mins(Map),
%%   {MX, MY} = maxes(Map),
%%   print({LX, LY}, {MX, MY}, Map).
