-module(incorrect_type).

%% API
%-compile([{parse_transform, session_check}]).
-compile([export_all]).

-register([
  {id1_fun, id1},
  {id2_fun, id2}
]).

-session({{id1, id2}, [
  {send, float},
  {recv, float},
  eot
]}).

main() ->
  PID1 = spawn(?MODULE, id1_fun, []),
  PID2 = spawn(?MODULE, id2_fun, []),
  register(id1, PID1),
  register(id2, PID2).

id1_fun() ->
  id2 ! {id1, 0},

  receive {id2, Val} when is_float(Val) ->
    io:fwrite("Result: ~p~n", [Val + 1.0])
  end.


id2_fun() ->
  receive {id1, Val} when is_float(Val) ->
    io:fwrite("Received: ~p~n", [Val]),
    id1 ! {id2, 1.0}
  end.