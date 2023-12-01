#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/1

-mode(compile).

main(Args) ->
  Input = read_lines(),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~p~n", [Ans]).

read_lines() -> read_lines([]).

read_lines(Acc) ->
  case io:get_line("") of
    eof -> lists:reverse(Acc);
    Res -> read_lines([Res|Acc])
  end.

solve_first(Input) ->
  Fun = fun(In, Acc) -> num(In) + Acc end,
  lists:foldl(Fun, 0, Input).

num(In) ->
  Fun =
    fun(C, O) when C >= $0, C =< $9 -> N = C - $0, upd(N, O);
       (_, O) -> O
    end,
  {D, U} = lists:foldl(Fun, {x, x}, In),
  D * 10 + U.

upd(N, {x, x}) -> {N, N};
upd(N, {A, _}) -> {A, N}.

solve_second(Input) ->
  Fun = fun(In, Acc) -> numcy(In) + Acc end,
  lists:foldl(Fun, 0, Input).

numcy(In) -> numcy(In, {x, x}).

numcy([], {D, U}) -> 10 * D + U;
numcy("one"   ++ _ = [_|R], O) -> numcy(R, upd(1, O));
numcy("two"   ++ _ = [_|R], O) -> numcy(R, upd(2, O));
numcy("three" ++ _ = [_|R], O) -> numcy(R, upd(3, O));
numcy("four"  ++ _ = [_|R], O) -> numcy(R, upd(4, O));
numcy("five"  ++ _ = [_|R], O) -> numcy(R, upd(5, O));
numcy("six"   ++ _ = [_|R], O) -> numcy(R, upd(6, O));
numcy("seven" ++ _ = [_|R], O) -> numcy(R, upd(7, O));
numcy("eight" ++ _ = [_|R], O) -> numcy(R, upd(8, O));
numcy("nine"  ++ _ = [_|R], O) -> numcy(R, upd(9, O));
numcy([C|R], O) when C >= $0, C =< $9 -> N = C - $0, numcy(R, upd(N, O));
numcy([_|R], O) -> numcy(R, O).
