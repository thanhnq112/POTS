-module(proc_sieve).

-export([gen_print/1, generate/1, sieve/1]).


sieve(State) ->
  {N, NextPid} = State,
    receive
    Value when is_integer(Value) ->
      if
        N == undefined ->
          NewN = Value,
          NewState = {NewN, NextPid},
          sieve(NewState);
        true ->
          Remainder = Value rem N,
          if
            Remainder == 0 ->
              sieve(State);
            true ->
              if
                NextPid == undefined ->
                  InitialState = {undefined, undefined},
                  NewNextPid = spawn(proc_sieve, sieve, [InitialState]),
                  NewNextPid ! Value,
                  NewState = {N, NewNextPid},
                  sieve(NewState);
                true ->
                  NextPid ! Value,
                  sieve(State)
              end
          end
      end;
    {done, RepId} ->
      if
        NextPid == undefined ->
          RepId ! [N],
          exit(normal);
        true -> NextPid ! {done, self()},
          receive
            Value when is_list(Value) ->
              RepId ! [N|Value],
              exit(normal)
          end
      end
  end.

generate(MaxN) ->
  List = lists:seq(2, MaxN),
  InitialState = {undefined, undefined},
  BasePid = spawn(proc_sieve, sieve, [InitialState]),
  lists:foreach(fun(X) -> BasePid ! X end, List),
  BasePid ! {done, self()},
  receive
    Value -> Value
  end.

gen_print(MaxN) -> generate(MaxN).