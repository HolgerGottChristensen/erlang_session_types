-module(test_pt).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
  ets:new(register, [named_table]),
  ets:new(session, [named_table]),
  ets:new(session_graphs, [named_table]),
  ets:new(current_session, [named_table]),

  parse_trans:plain_transform(fun do_transform/1, Forms),

  lists:foreach(
    fun({K, V}) ->
      Graph = digraph:new(),
      digraph:add_vertex(Graph, initial),
      create_fsm(Graph, V, initial),
      ets:insert(session_graphs, {K, Graph})
    end, ets:tab2list(session)
  ),

  lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(register)),
  lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(session)),

  lists:foreach(
    fun({K, Graph}) ->
      io:fwrite("==== Graph: ~p ====~n", [K]),
      {_, Vertices, Edges, _Neighbours, _Cyclic} = Graph,
      io:fwrite("Nodes: ~n", []),
      lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(Vertices)),

      io:fwrite("Edges: ~n", []),
      lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(Edges)),
      io:fwrite("========================~n~n", [])
    end, ets:tab2list(session_graphs)
  ),


  parse_trans:plain_transform(fun check_sessions/1, Forms),
  io:fwrite("~s~n", [color:green("Session types type checks!")]),
  Forms.



check_function_session(Self, {op, L1, '!', {atom, L2, To}, {tuple, L3, [{atom, L4, Self}, {Ty, L5, V}]}}) ->
  %lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(current_session)),

  [[Graph]] = ets:match(session_graphs, {{Self, To}, '$0'}),
  [[CurrentState]] = ets:match(current_session, {To, '$0'}),
  % Check the type of the send matches with the session.

  OutEdges = digraph:out_edges(Graph, CurrentState),
  OutEdgesMapped = lists:map(fun (G) -> digraph:edge(Graph, G) end, OutEdges),

  K = lists:search(
    fun(T) ->
      case T of
        {_, _, _, [{send, Ty}]} -> true;
        _ -> false
      end
    end, OutEdgesMapped),

  case K of
    {value, {_, _, ResState, _}} ->
      %io:fwrite("Found: ~p~n", [ResState]),
      ets:insert(current_session, {To, ResState}),
      %io:fwrite("Send: ~p, To: ~p~n~n", [V, To]),

      %lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(current_session)),
      {op, L1, '!', {atom, L2, To}, {tuple, L3, [{atom, L4, Self}, {Ty, L5, V}]}};
    false ->
      {error, io:format("~s~w~n", [color:red("Could not find a suitable send edge in the fsm with type: "), Ty])}
  end;
check_function_session(Self, {'receive', L1, [{clause, L2, [{tuple, L3, [{atom, L4, From}, {var, L5, Val}]}], [[{call, L6, {atom, L7, TyFun}, [{var, L8, Val}]}]], Body}]}) ->
  %io:fwrite("Receive: ~p ~p ~p~n~n", [From, TyFun, Val]),
  %lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(current_session)),

  [[Graph]] = ets:match(session_graphs, {{Self, From}, '$0'}),
  [[CurrentState]] = ets:match(current_session, {From, '$0'}),
  % Check the type of the send matches with the session.

  OutEdges = digraph:out_edges(Graph, CurrentState),
  OutEdgesMapped = lists:map(fun (G) -> digraph:edge(Graph, G) end, OutEdges),

  Ty = case TyFun of
         is_integer -> integer;
         is_atom -> atom;
         is_float -> float;
         is_boolean -> boolean;
         is_bitstring -> string;
         _ -> error
       end,

  K = lists:search(
    fun(T) ->
      case T of
        {_, _, _, [{recv, Ty}]} -> true;
        _ -> false
      end
    end, OutEdgesMapped),

  case K of
    {value, {_, _, ResState, _}} ->
      %io:fwrite("Found: ~p~n", [ResState]),
      ets:insert(current_session, {From, ResState}),
      %io:fwrite("Send: ~p, To: ~p~n~n", [V, From]),

      %lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(current_session)),
      parse_trans:plain_transform(fun (A) -> check_function_session(Self, A) end, Body),

      {'receive', L1, [{clause, L2, [{tuple, L3, [{atom, L4, From}, {var, L5, Val}]}], [[{call, L6, {atom, L7, TyFun}, [{var, L8, Val}]}]], Body}]};
    false ->
      {error, io:format("~s~w~n", [color:red("Could not find a suitable recv edge in the fsm with type: "), Ty])}
  end;
check_function_session(Self, {op, _, '!', Receiver, {tuple, _, [{atom, _, _}, {_, _, _}]}}) ->
  {error, io:format("~s~w~n", [color:red("The thing sent to, is required to be an atom but was: "), Receiver])};
check_function_session(Self, {op, _, '!', _, {tuple, _, [Error, {Ty, _, V}]}}) ->
  {error, io:format("~s~w~n", [color:red("The message sent, must contain the receiver id as the first element: "), Error])};
check_function_session(Self, {op, _, '!', _, {tuple, _, [_, Error]}}) ->
  {error, io:format("~s~w~n", [color:red("We only support sending primitive values: "), Error])};
check_function_session(Self, {op, _, '!', _, Error}) ->
  {error, io:format("~s~w~n", [color:red("The thing to be sent must be a tuple: "), Error])};
check_function_session(Self, {op, _, Error, _, _}) ->
  {error, io:format("~s~w~n", [color:red("Only the ! operator is supported: "), Error])};
check_function_session(Self, {call, L1, Error, L2}) ->
  io:fwrite("~s~w~n", [color:yellow("Function calls are currently not considered: "), Error]),
  {call, L1, Error, L2};
check_function_session(Self, T) ->
  io:fwrite("Not matched against: ~p~n~n", [T]),
  continue.

check_sessions({function, _, Name, 0, Body}) ->
  case ets:match(register, {Name, '$0'}) of
    [] -> continue;
    [[T]] ->
      ets:delete_all_objects(current_session),
      lists:foreach(
        fun({{K1, K2}, _}) ->
          if
            K1 == T -> ets:insert(current_session, {K2, initial});
            true -> ok
          end
        end, ets:tab2list(session_graphs)
      ),
      io:fwrite("=== Check Session for: ~p ~p ===~n", [Name, T]),
      lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(current_session)),
      io:fwrite("======~n~n", []),
      parse_trans:plain_transform(fun (A) -> check_function_session(T, A) end, Body),

      % Check that all current states are in a final state
      InFinal = lists:filter(
        fun ({To, CurrentState}) ->
          [[Graph]] = ets:match(session_graphs, {{T, To}, '$0'}),
          case digraph:vertex(Graph, CurrentState) of
              {_, [final]} -> false;
              _ -> true
          end
        end, ets:tab2list(current_session)),
      case InFinal of
        [] -> continue;
        B -> {error, io:format("~s~w~n", [color:red("The states are not in a final state: "), B])}
      end
  end;
check_sessions({function, _, _, Error, _}) ->
  {error, io:format("~s~w~n", [color:red("Only functions of arity 0 are allowed: "), Error])};
check_sessions(T) ->
  %io:fwrite("Not considered: ~p~n~n", [T]),
  continue.

do_transform({attribute, _, session, {{Key1, Key2}, Value}}) ->
  ets:insert(session, {{Key1, Key2}, Value}),
  ets:insert(session, {{Key2, Key1}, dualize(Value)}),
  io:fwrite("Session Attribute: ~p~n~n", [Value]),
  continue;
do_transform({attribute, _, register, Value}) ->
  ets:insert(register, Value),
  io:fwrite("Register Attribute: ~p~n~n", [Value]),
  continue;
do_transform(T) ->
  %io:fwrite("Not considered: ~p~n~n", [T]),
  continue.

dualize(SessionType) ->
  lists:map(
    fun (T) ->
      case T of
        {send, U} -> {recv, U};
        {recv, U} -> {send, U};
        eot -> eot
      end
    end, SessionType).

create_fsm(Graph, [Head | Tail], CurrentNode) ->
  case Head of
    {send, T} ->
      NewNode = digraph:add_vertex(Graph),
      digraph:add_edge(Graph, CurrentNode, NewNode, [{send, T}]),
      create_fsm(Graph, Tail, NewNode);
    {recv, T} ->
      NewNode = digraph:add_vertex(Graph),
      digraph:add_edge(Graph, CurrentNode, NewNode, [{recv, T}]),
      create_fsm(Graph, Tail, NewNode);
    eot ->
      digraph:add_vertex(Graph, CurrentNode, [final])
  end;
create_fsm(_Graph, [], _CurrentNode) ->
  ok.