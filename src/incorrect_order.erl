-module(incorrect_order).

%% API
%-compile([{parse_transform, session_check}]).
-compile([export_all]).

-register([
  {id1_fun, id1},
  {id2_fun, id2}
]).

-session({{id1, id2}, [
  {send, atom},
  {recv, atom},
  eot
]}).

main() ->
  PID1 = spawn(?MODULE, id1_fun, []),
  PID2 = spawn(?MODULE, id2_fun, []),
  register(id1, PID1),
  register(id2, PID2).

id1_fun() ->
  receive {id2, Val} when is_atom(Val)->
    io:fwrite("Shutting down id1")
  end,
  id2 ! {id1, shutdown}.

id2_fun() ->
  receive {id1, Val} when is_atom(Val) ->
    io:fwrite("Shutting down id2"),
    id1 ! {id2, shutdown}
  end.