#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/19

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
  Lines = read_lines(),
  Fold =
    fun("\n", Acc) -> Acc;
       (Line, {Rs, Ps}) ->
        [H|_] = string:split(Line, "}"),
        case string:split(H, "{") of
          ["", Part] ->
            Cs = string:split(Part,",",all),
            P = [{[C],list_to_integer(T)} || [C,$=|T] <- Cs],
            {Rs, [maps:from_list(P)|Ps]};
          [Name, Proc] ->
            Ops = string:split(Proc,",",all),
            Map =
              fun(Op) ->
                  case string:split(Op,":") of
                    [Dst] -> {gt, Dst};
                    [[C,Cmp|Num],Dst] ->
                      {cmp, [C], [Cmp], list_to_integer(Num), Dst}
                  end
              end,
            Prog = [Map(Op) || Op <- Ops],
            {Rs#{Name => Prog}, Ps}
        end
    end,
  {Rs, Ps} = lists:foldl(Fold, {#{}, []}, Lines),
  {Rs, lists:reverse(Ps)}.

read_lines() -> read_lines([]).

read_lines(Acc) ->
  case io:get_line("") of
    eof -> lists:reverse(Acc);
    Res -> read_lines([Res|Acc])
  end.

solve_first({Rs, Ps}) ->
  Filter = [P || P <- Ps, accept("in", P, Rs)],
  Fold =
    fun(P, Acc) ->
        Acc + lists:sum([N || {_, N} <- maps:to_list(P)])
    end,
  lists:foldl(Fold, 0, Filter).

accept("A", _, _) -> true;
accept("R", _, _) -> false;
accept(Proc, P, Rs) ->
  #{Proc := Prog} = Rs,
  NProc = match(Prog, P),
  accept(NProc, P, Rs).

match([{gt, L}|_], _) -> L;
match([{cmp, C, Op, V, Dst}|R], P) ->
  #{C := VV} = P,
  Res =
    case Op of
      ">" -> VV > V;
      "<" -> VV < V
    end,
  case Res of
    true -> Dst;
    false -> match(R, P)
  end.

solve_second({Rs, _}) ->
  State = maps:from_list([{[C], {1, 4000}} || C <- "xmas"]),
  explore(queue:from_list([{"in", State}]), Rs, 0).

explore(Q, Rs, Acc) ->
  {V, NQ} = queue:out(Q),
  case V of
    empty -> Acc;
    {value, {Proc, State}} ->
      case Proc of
        "A" -> explore(NQ, Rs, Acc + mul(State));
        "R" -> explore(NQ, Rs, Acc);
        _ ->
          #{Proc := Prog} = Rs,
          FQ = process(Prog, State, NQ),
          explore(FQ, Rs, Acc)
      end
  end.

mul(State) ->
  Rs = [H - L + 1 || {_, {L, H}} <- maps:to_list(State)],
  lists:foldl(fun erlang:'*'/2, 1, Rs).

process([{gt, L}|_], State, Q) ->
  queue:in({L, State}, Q);
process([Op|R], State, Q) ->
  {ST, SF} = split(Op, State),
  %%io:format("~p ->~n ~p ->~n  ~p~n  ~p~n",[maps:to_list(State), Op, ST, SF]),
  NQ =
    case ST of
      false -> Q;
      {L, S} -> queue:in({L, S}, Q)
    end,
  case SF =:= false of
    true -> NQ;
    false -> process(R, SF, NQ)
  end.

split({cmp, C, Op, V, Dst}, State) ->
  #{C := {L, H}} = State,
  {RT, RF} =
    case Op of
      "<" ->
        case L > V of
          true -> {false, {L, H}};
          false ->
            case H < V of
              true -> {{L, H}, false};
              false -> {{L, V - 1}, {V, H}}
            end
        end;
      ">" ->
        case L > V of
          true -> {{L, H}, false};
          false ->
            case H < V of
              true -> {false, {L, H}};
              false -> {{V + 1, H}, {L, V}}
            end
        end
    end,
  ST =
    case RT =:= false of
      true -> false;
      false -> {Dst, State#{C => RT}}
    end,
  SF =
    case RF =:= false of
      true -> false;
      false -> State#{C => RF}
    end,
  {ST, SF}.
