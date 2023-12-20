#!/usr/bin/env escript

%% https://adventofcode.com/2023/day/20

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
  Tokens = read_many("~s"),
  Fold =
    fun("->", Acc) -> Acc;
       (RToken, {Cur, Acc}) ->
        Token = RToken -- ",",
        case is_init(Token) of
          {ok, Name, Type} ->
            #{name := Oname, dst := Dst} = Cur,
            {#{ name => Name
              , type => Type
              , dst  => []
              , mem => #{}
              },
             Acc#{Oname => Cur#{dst => lists:reverse(Dst)}}};
          false ->
            #{dst := Dst} = Cur,
            {Cur#{dst => [Token|Dst]}, Acc}
        end
    end,
  {_, Acc} = lists:foldl(Fold, {#{name => "fake", dst => []}, #{}}, Tokens),
  Acc.

read(Pat) ->
  io:fread("", Pat).

read_many(Pat) ->
  read_many(Pat, []).

read_many(Pat, Acc) ->
  case read(Pat) of
    {ok, R} -> read_many(Pat, [R|Acc]);
    _ -> lists:append(lists:reverse([["%fin"]|Acc]))
  end.

is_init("broadcaster" = N) ->
  {ok, N, broadcaster};
is_init([C|R]) ->
  case C of
    $% ->
      {ok, R, flip};
    $& ->
      {ok, R, con};
    _ -> false
  end.

solve_first(RInput) ->
  Input = init_con_mem(RInput),
  push(1000, Input, {0, 0}).

init_con_mem(RInput) ->
  Fold =
    fun(Who, V, State) ->
        Dst = maps:get(dst, V, []),
        Fold1 =
          fun(D, Acc) ->
              DState = maps:get(D, Acc, #{}),
              DMem = maps:get(mem, DState, #{}),
              Acc#{D => DState#{mem => DMem#{Who => false}}}
          end,
        lists:foldl(Fold1, State, Dst)
    end,
  maps:fold(Fold, RInput, RInput).

push(0, _, {L, H}) -> L * H;
push(N, State, {L, H}) ->
  Q = queue:from_list([{"broadcaster", {low, "button"}}]),
  {NState, NAcc} = flow(Q, State, {L + 1, H}),
  push(N - 1, NState, NAcc).

flow(Q, State, {L, H}) ->
  {V, NQ} = queue:out(Q),
  case V of
    empty -> {State, {L, H}};
    {value, {Who, What}} ->
      %%io:format("~n~p~n", [{Who, What}]),
      WState = maps:get(Who, State, #{}),
      Dst = maps:get(dst, WState, []),
      Mem = maps:get(mem, WState, #{}),
      Type = maps:get(type, WState, fake),
      {NWState, FQ, NL, NH} =
        case {Type, What} of
          {flip, {low, _}} ->
            S = maps:get(s, Mem, off),
            {P, NS, LM, HM} =
              case S of
                off -> {high, on, 0, 1};
                on -> {low, off, 1, 0}
              end,
            NMem = #{s => NS},
            FIQ = lists:foldl(fun queue:in/2, NQ, [{D, {P, Who}} || D <- Dst]),
            NIL = L + LM * length(Dst),
            HIL = H + HM * length(Dst),
            %%io:format("  ~p~n", [{flip, Who, NS}]),
            {State#{Who => WState#{mem => NMem}}, FIQ, NIL, HIL};
          {con, {Lvl, From}} ->
            NWS = if Lvl == high -> true; true -> false end,
            NMem = Mem#{From => NWS},
            Fold = fun(_, Vi, Acc) -> Acc and Vi end,
            Out = maps:fold(Fold, true, NMem),
            {P, LM, HM} =
              case Out of
                true -> {low, 1, 0};
                false -> {high, 0, 1}
              end,
            case Who =:= "ls" of
              true ->
                Fold1 = fun(_, V, Acc) -> Acc or V end,
                case maps:fold(Fold1, false, NMem) of
                  true -> io:format("~p ~p~n~n", [get(n), NMem]);
                  false -> ok
                end;
              false ->
                ok
            end,

            FIQ = lists:foldl(fun queue:in/2, NQ, [{D, {P, Who}} || D <- Dst]),
            NIL = L + LM * length(Dst),
            HIL = H + HM * length(Dst),
            %%io:format("  ~p~n", [{con, Who, NMem, P}]),
            {State#{Who => WState#{mem => NMem}}, FIQ, NIL, HIL};
          {broadcaster, {Lvl, _}} ->
            {LM, HM} =
              case Lvl of
                high -> {0, 1};
                low -> {1, 0}
              end,
            FIQ =
              lists:foldl(fun queue:in/2, NQ, [{D, {Lvl, Who}} || D <- Dst]),
            NIL = L + LM * length(Dst),
            HIL = H + HM * length(Dst),
            {State, FIQ, NIL, HIL};
          _ -> {State, NQ, L, H}
        end,
      %%io:format("~p ->~n  ~p ->~n    ~p~n", [{L,H,State}, V, {NL, NH, NWState}]),
      flow(FQ, NWState, {NL, NH})
  end.

-define(L, 10000).

%% rx is connected to a cojunction gate with a few other conj inputs
%% In order for it to fire, all its inputs needs to fire high.
%% Simulate 10000 steps, see when at least one input fires
%% Each seems to fire on a cycle.
%% Calculate the LCM of them.
%%
%% Code only helps to find the cycles.

solve_second(RInput) ->
  Input = init_con_mem(RInput),
  push1(?L, Input, {0, 0}).

push1(0, _, {L, H}) -> {L, H};
push1(N, State, {L, H}) ->
  Q = queue:from_list([{"broadcaster", {low, "button"}}]),
  put(n, ?L - N + 1),
  {NState, NAcc} = flow(Q, State, {L + 1, H}),
  push1(N - 1, NState, NAcc).
