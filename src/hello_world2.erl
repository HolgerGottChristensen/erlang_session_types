-module(hello_world2).

%% API
%-compile([{parse_transform, test_pt}]).
-compile([export_all]).

-register([
  {hello1, id1},
  {hello2, id2},
  {hello3, id3}
]).

-session({{id1, id2}, [
  {send, integer},
  {send, atom},
  {recv, integer},
  eot
]}).

-session({{id1, id3}, [
  {choose, [
    {ok, [
      {send, integer},
      eot
    ]},

    {error, [
      {send, atom},
      eot
    ]}
  ]}
]}).

main() ->
  PID1 = spawn(?MODULE, hello1, []),
  PID2 = spawn(?MODULE, hello2, []),
  PID3 = spawn(?MODULE, hello3, []),
  register(id1, PID1),
  register(id2, PID2),
  register(id3, PID3).
% a(x).b(y). !x . ?y         |        /a(z)./b(t). ?z.  !t

hello1() ->
  id2 ! {id1, 42},
  case false of
    false -> id2 ! {id1, nice};
    true -> id2 ! {id1, nice}
  end,
  if
    false -> id3 ! {id1, ok}, id3 ! {id1, 42};
    true -> id3 ! {id1, error}, id3 ! {id1, test}
  end,
  receive {id2, Val} when is_integer(Val) ->
    io:fwrite("Forms = ~p~n", [Val])
  end.

hello2() ->
  receive {id1, Val} when is_integer(Val) ->
    io:fwrite("Forms = ~p~n", [Val]),

    receive {id1, Val2} when is_atom(Val2) ->
      io:fwrite("Forms = ~p~n", [Val2])
    end

  end,
  id1 ! {id2, 42}.

hello3() ->
  receive
    {id1, ok} ->
      receive {id1, Val} when is_integer(Val) ->
        io:fwrite("Forms = ~p~n", [Val])
      end;
    {id1, error} ->
      receive {id1, Val} when is_atom(Val) ->
        io:fwrite("Forms = ~p~n", [Val])
      end
  end
.

parse() ->
  {ok, AST} = epp:parse_file("src/hello_world.erl", [{includes, "include"}]),
  AST.

