-module(hello_world).

%% API
%-compile([{parse_transform, session_check}]).
-compile([export_all]).

-register([
  {hello1, id1},
  {hello2, id2}
]).

-session({{id1, id2}, [
  {label, lab1},
  {send, integer},
  {goto, lab1}
]}).

main() ->
  PID1 = spawn(?MODULE, hello1, []),
  PID2 = spawn(?MODULE, hello2, []),
  register(id1, PID1),
  register(id2, PID2).

hello1() ->
  id2 ! {id1, 42},
  hello1().

hello2() ->
  receive {id1, Val} when is_integer(Val) ->
    io:fwrite("Forms = ~p~n", [Val])
  end,
  receive {id1, Val} when is_integer(Val) ->
    io:fwrite("Forms = ~p~n", [Val])
  end,
  hello2().

