#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/5

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
  read("seeds:"),
  Seeds = lists:append(read_many("~d")),
  Maps = read_maps(),
  {Seeds, Maps}.

read(Pat) ->
  io:fread("", Pat).

read_many(Pat) ->
  read_many(Pat, []).

read_many(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, R} -> read_many(Pat, [R|Acc]);
    _ -> lists:reverse(Acc)
  end.

read_maps() ->
  read_maps([]).

read_maps(Acc) ->
  case read_map() of
    {ok, M} -> read_maps([M|Acc]);
    eof -> lists:reverse(Acc)
  end.

read_map() ->
  case read_many("~d ~d ~d") of
    [] -> eof;
    Triads ->
      {ok, lists:sort([{S, R, D} || [D, S, R] <- Triads])}
  end.

solve_first({Seeds, Maps}) ->
  [H|_] = lists:sort([map(S, Maps) || S <- Seeds]),
  H.

map(S, Maps) ->
  Fold = fun(Map, N) -> map1(N, Map) end,
  lists:foldl(Fold, S, Maps).

map1(N, []) -> N;
map1(N, [{S, _, _}|_]) when S > N -> N;
map1(N, [{S, R, D}|Rest]) ->
  case S + R < N of
    true -> map1(N, Rest);
    false -> D - S + N
  end.

solve_second({Seeds, Maps}) ->
  ToPairs = lists:sort(to_pairs(Seeds)),
  [{S, _}|_] = map_all(ToPairs, Maps),
  S.

to_pairs(S) ->
  to_pairs(S, []).

to_pairs([], Acc) -> lists:reverse(Acc);
to_pairs([A,B|R], Acc) -> to_pairs(R, [{A, A + B - 1}|Acc]).

map_all(Ranges, Maps) ->
  Fold = fun(Map, Rs) -> map_all1(Rs, Map) end,
  lists:foldl(Fold, Ranges, Maps).

map_all1(Ranges, Map) ->
  merge(map_all1(Ranges, Map, [])).

map_all1([], _, Acc) -> lists:sort(Acc);
map_all1(Rs, [], Acc) -> map_all1([], [], Rs ++ Acc);
map_all1([{B, E}|Rest] = Rs, [{BB, L, D}|M] = Ms, Acc) ->
  EE = BB + L - 1,
  if
    BB > E -> map_all1(Rest, Ms, [{B,E}|Acc]);
    B > EE -> map_all1(Rs, M, Acc);
    true ->
      Di = D - BB,
      if
        BB > B, EE < E ->
          map_all1([{EE + 1, E}|Rest], M, [{B, BB - 1},{BB + Di, EE + Di}|Acc]);
        B > BB, E < EE ->
          map_all1(Rest, Ms, [{B + Di, E + Di}|Acc]);
        B > BB, B =< EE ->
          map_all1([{EE + 1, E}|Rest], M, [{B + Di, EE + Di}|Acc]);
        true ->
          map_all1(Rest, Ms, [{B, BB - 1}, {BB + Di, E + Di}|Acc])
      end
  end.

merge(Rs) ->
  merge(Rs, []).

merge([X], Acc) -> lists:reverse([X|Acc]);
merge([{A, B} = X, {C, D} = Y|R], Acc) ->
  if
    B + 1 < C -> merge([Y|R], [X|Acc]);
    D =< B -> merge([X|R], Acc);
    true -> merge([{A, D}|R], Acc)
  end.
