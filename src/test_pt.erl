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



check_function_session(Self, CurrentStates, {op, _, '!', {atom, _, To}, {tuple, _, [{atom, _, Self}, {Ty, _, V}]}}) ->
  %io:fwrite("Send: ~n", []),
  %lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(current_session)),

  [[Graph]] = ets:match(session_graphs, {{Self, To}, '$0'}),
  {ok, CurrentState} = dict:find(To, CurrentStates),
  % Check the type of the send matches with the session.

  OutEdges = digraph:out_edges(Graph, CurrentState),
  OutEdgesMapped = lists:map(fun (G) -> digraph:edge(Graph, G) end, OutEdges),

  K = lists:search(
    fun(T) ->
      case T of
        {_, _, _, [{send, Ty}]} -> true;
        {_, _, _, [{choose, V}]} -> true;
        _ -> false
      end
    end, OutEdgesMapped),

  case K of
    {value, {_, _, ResState, _}} ->
      %io:fwrite("Found: ~p~n", [ResState]),
      NewCurrentStates = dict:store(To, ResState, CurrentStates),
      %io:fwrite("Send: ~p, To: ~p~n~n", [V, To]),

      %lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(current_session)),
      {ok, NewCurrentStates};
    false ->
      {error, io:format("~s~w. Found; ~w~n", [color:red("Could not find a suitable send edge in the fsm with type: "), Ty, OutEdgesMapped])}
  end;
check_function_session(Self, CurrentStates, {'receive', _, [{clause, _, [{tuple, _, [{atom, _, From}, {var, _, Val}]}], [[{call, _, {atom, _, TyFun}, [{var, _, Val}]}]], Body}]}) ->
  %io:fwrite("Receive: ~p ~p ~p~n~n", [From, TyFun, Val]),
  %lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(current_session)),

  [[Graph]] = ets:match(session_graphs, {{Self, From}, '$0'}),
  {ok, CurrentState} = dict:find(From, CurrentStates),
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
      NewCurrentStates = dict:store(From, ResState, CurrentStates),
      %io:fwrite("Send: ~p, To: ~p~n~n", [V, From]),

      %lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(current_session)),
      NewCurrentStates2 = check_function_session(Self, NewCurrentStates, Body),

      NewCurrentStates2;
    false ->
      {error, io:format("~s~w~n", [color:red("Could not find a suitable recv edge in the fsm with type: "), Ty])}
  end;
check_function_session(_, _, {op, _, '!', Receiver, {tuple, _, [{atom, _, _}, {_, _, _}]}}) ->
  {error, io:format("~s~w~n", [color:red("The thing sent to, is required to be an atom but was: "), Receiver])};
check_function_session(_, _, {op, _, '!', _, {tuple, _, [Error, {Ty, _, V}]}}) ->
  {error, io:format("~s~w~n", [color:red("The message sent, must contain the receiver id as the first element: "), Error])};
check_function_session(_, _, {op, _, '!', _, {tuple, _, [_, Error]}}) ->
  {error, io:format("~s~w~n", [color:red("We only support sending primitive values: "), Error])};
check_function_session(_, _, {op, _, '!', _, Error}) ->
  {error, io:format("~s~w~n", [color:red("The thing to be sent must be a tuple: "), Error])};
check_function_session(_, _, {op, _, Error, _, _}) ->
  {error, io:format("~s~w~n", [color:red("Only the ! operator is supported: "), Error])};
check_function_session(_, CurrentStates, {call, _, Error, _}) ->
  io:fwrite("~s~w~n", [color:yellow("Function calls are currently not considered: "), Error]),
  {ok, CurrentStates};
check_function_session(Self, CurrentStates, [StmtHd | StmtTail]) ->
  lists:foldl(
    fun (Elem, Acc) ->
      case Acc of
        {ok, New} -> check_function_session(Self, New, Elem);
        A -> A
      end
    end, {ok, CurrentStates}, [StmtHd | StmtTail]);
check_function_session(Self, CurrentStates, {clause, _, _, _, Body}) ->
  check_function_session(Self, CurrentStates, Body);
check_function_session(Self, CurrentStates, {'if', _, Clauses}) ->
  Mapped = lists:map(fun ({clause, _, _, _, Body}) -> check_function_session(Self, CurrentStates, Body) end, Clauses),
  V = lists:all(fun (Elem) -> Elem == lists:nth(1, Mapped) end, Mapped),
  if
    V -> lists:nth(1, Mapped);
    true -> {error, io:format("~s~w~n", [color:red("All if branches are not equal: "), Mapped])}
  end;
check_function_session(Self, CurrentStates, {'case', _, _, Clauses}) ->
  Mapped = lists:map(fun ({clause, _, _, _, Body}) -> check_function_session(Self, CurrentStates, Body) end, Clauses),
  V = lists:all(fun (Elem) -> Elem == lists:nth(1, Mapped) end, Mapped),
  if
    V -> lists:nth(1, Mapped);
    true -> {error, io:format("~s~w~n", [color:red("All cases are not equal: "), Mapped])}
  end;
check_function_session(_, _, T) ->
  {error, io:format("~s~p~n~n", [color:red("Not matched against: "), T])}.

check_sessions({function, _, Name, 0, Body}) ->
  case ets:match(register, {Name, '$0'}) of
    [] -> continue;
    [[T]] ->
      % Create a list of things to insert into the state
      RelevantList = lists:filter(fun({{K1, K2}, _}) -> K1 == T end, ets:tab2list(session_graphs)),
      MappedRelevantList = lists:map(fun({{K1, K2}, _}) -> {K2, initial} end, RelevantList),

      % Create dict from list
      CurrentStates = dict:from_list(MappedRelevantList),

      io:fwrite("=== Check Session for: ~p ~p ===~n", [Name, T]),
      lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, dict:to_list(CurrentStates)),
      io:fwrite("======~n", []),
      %io:fwrite("~p~n", [Body]),
      %io:fwrite("======~n~n", []),

      case check_function_session(T, CurrentStates, Body) of
        {ok, FinalState} ->
          % Check that all current states are in a final state
          InFinal = lists:filter(
            fun ({To, CurrentState}) ->
              [[Graph]] = ets:match(session_graphs, {{T, To}, '$0'}),
              case digraph:vertex(Graph, CurrentState) of
                {_, [final]} -> false;
                _ -> true
              end
            end, dict:to_list(FinalState)),
          case InFinal of
            [] -> continue;
            B -> {error, io:format("~s~w~n", [color:red("The states are not in a final state: "), B])}
          end;
        {error, Message} ->
          {error, Message}
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
        {offer, U} -> {choose, lists:map(fun ({Label, S}) -> {Label, dualize(S)} end, U)};
        {choose, U} -> {offer, lists:map(fun ({Label, S}) -> {Label, dualize(S)} end, U)};
        eot -> eot
      end
    end, SessionType).


create_fsm(Graph, [eot | Tail], CurrentNode) ->
  digraph:add_vertex(Graph, CurrentNode, [final]);

create_fsm(Graph, [{send, T} | Tail], CurrentNode) ->
  NewNode = digraph:add_vertex(Graph),
  digraph:add_edge(Graph, CurrentNode, NewNode, [{send, T}]),
  create_fsm(Graph, Tail, NewNode);
create_fsm(Graph, [{recv, T} | Tail], CurrentNode) ->
  NewNode = digraph:add_vertex(Graph),
  digraph:add_edge(Graph, CurrentNode, NewNode, [{recv, T}]),
  create_fsm(Graph, Tail, NewNode);
create_fsm(Graph, [{choose, T} | Tail], CurrentNode) ->
  % Create a state machine for each label
  lists:map(
    fun ({Label, S}) ->
      NewNode = digraph:add_vertex(Graph),
      digraph:add_edge(Graph, CurrentNode, NewNode, [{choose, Label}]),
      create_fsm(Graph, S, NewNode)
    end, T);

create_fsm(Graph, [{offer, T}], CurrentNode) ->
  % Create a state machine for each label
  lists:map(
    fun ({Label, S}) ->
      NewNode = digraph:add_vertex(Graph),
      digraph:add_edge(Graph, CurrentNode, NewNode, [{offer, Label}]),
      create_fsm(Graph, S, NewNode)
    end, T);

create_fsm(_Graph, [], CurrentNode) ->
  CurrentNode.