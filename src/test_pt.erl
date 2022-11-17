-module(test_pt).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
  io:fwrite("~n~s~n", [color:blue("Performing type check of session types...")]),

  ets:new(register, [named_table]),
  ets:new(session, [named_table]),
  ets:new(session_graphs, [named_table]),
  ets:new(current_session, [named_table]),

  parse_trans:plain_transform(fun do_transform/1, Forms),
  io:fwrite("~s~n~n", [color:blue("Done collecting session and register attributes. Building graphs...")]),

  lists:foreach(
    fun({K, V}) ->
      Graph = digraph:new(),
      digraph:add_vertex(Graph, initial),
      create_fsm(Graph, V, initial),
      ets:insert(session_graphs, {K, Graph})
    end, ets:tab2list(session)
  ),

  %lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(register)),
  %lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(session)),

  lists:foreach(
    fun({K, Graph}) ->
      io:fwrite("==== Graph: ~p ====~n", [K]),
      {_, Vertices, Edges, _Neighbours, _Cyclic} = Graph,
      io:fwrite("Nodes: ~n", []),
      lists:foreach(
        fun(G) ->
          case G of
            {[_ | Node], Label} -> io:fwrite("\tn~p ~p~n", [Node, Label]);
            {Node, Label} -> io:fwrite("\t~p ~p~n", [Node, Label])
          end
        end, ets:tab2list(Vertices)),

      io:fwrite("Edges: ~n", []),
      lists:foreach(
        fun(G) ->
          case G of
            {[_ | Edge], [_ | From], [_ | To], Label} -> io:fwrite("\te~p: n~p -> n~p ~p~n", [Edge, From, To, Label]);
            {[_ | Edge], From, [_ | To], Label} -> io:fwrite("\te~p: ~p -> n~p ~p~n", [Edge, From, To, Label]);
            {[_ | Edge], [_ | From], To, Label} -> io:fwrite("\te~p: n~p -> ~p ~p~n", [Edge, From, To, Label]);
            {[_ | Edge], From, To, Label} -> io:fwrite("\te~p: ~p -> ~p ~p~n", [Edge, From, To, Label])
          end
        end, ets:tab2list(Edges)),
      io:fwrite("========================~n~n", [])
    end, ets:tab2list(session_graphs)
  ),

  io:fwrite("~s~n~n", [color:blue("Graphs built. Performing session checks...")]),


  parse_trans:plain_transform(fun check_sessions/1, Forms),
  io:fwrite("~n~s~n~n", [color:greenb("Session types type checks!")]),
  Forms.


check_function_session(_, {error, S}, _) ->
  {error, S};
check_function_session(Self, {ok, CurrentStates}, {op, _, '!', {atom, _, To}, {tuple, _, [{atom, _, Self}, {Ty, _, V}]}}) ->
  %io:fwrite("Send: ~n", []),
  %lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(current_session)),



  [[Graph]] = ets:match(session_graphs, {{Self, To}, '$0'}),
  {ok, CurrentState} = dict:find(To, CurrentStates),
  ProgressedCurrentState = progress_state(Self, {ok, CurrentState}, To),

  % Check the type of the send matches with the session.

  OutEdges = digraph:out_edges(Graph, ProgressedCurrentState),
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
      Possible = lists:map(fun ({_, _, _, [B]}) -> B end, OutEdgesMapped),
      {error, io:format("~s~w~s~w~n", [color:red("Could not find a suitable send edge in the fsm with type: "), Ty, color:red(". Exprected one of: "), Possible])}
  end;
check_function_session(Self, {ok, CurrentStates}, {'receive', _, [{clause, _, [{tuple, _, [{atom, _, From}, {var, _, Val}]}], [[{call, _, {atom, _, TyFun}, [{var, _, Val}]}]], Body}]}) ->
  %io:fwrite("Receive: ~p ~p ~p~n~n", [From, TyFun, Val]),
  %lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(current_session)),

  [[Graph]] = ets:match(session_graphs, {{Self, From}, '$0'}),
  {ok, CurrentState} = dict:find(From, CurrentStates),
  ProgressedCurrentState = progress_state(Self, {ok, CurrentState}, From),

  % Check the type of the send matches with the session.

  OutEdges = digraph:out_edges(Graph, ProgressedCurrentState),
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
      NewCurrentStates2 = check_function_session(Self, {ok, NewCurrentStates}, Body),

      NewCurrentStates2;
    false ->
      Possible = lists:map(fun ({_, _, _, [B]}) -> B end, OutEdgesMapped),
      {error, io:format("~s~w~s~w~n", [color:red("Could not find a suitable recv edge in the fsm with type: "), Ty, color:red(". Exprected one of: "), Possible])}
  end;
check_function_session(Self, {ok, CurrentStates}, {'receive', _, [{clause, _, [{tuple, _, [{atom, _, From}, {atom, _, Offer}]}], _, Body} | TailClauses]}) ->
  % Check if the clause thing matches
  [[Graph]] = ets:match(session_graphs, {{Self, From}, '$0'}),
  {ok, CurrentState} = dict:find(From, CurrentStates),
  ProgressedCurrentState = progress_state(Self, {ok, CurrentState}, From),

  % Check the type of the send matches with the session.

  OutEdges = digraph:out_edges(Graph, ProgressedCurrentState),
  OutEdgesMapped = lists:map(fun (G) -> digraph:edge(Graph, G) end, OutEdges),

  Offers = [{Offer, Body}] ++ lists:map(fun ({clause, _, [{tuple, _, [{atom, _, _}, {atom, _, Offer2}]}], _, Body2}) -> {Offer2, Body2} end, TailClauses),

  ExpectedOffers = lists:filtermap(
    fun(T) ->
      case T of
        {_, _, ResState, [{offer, V}]} -> {true, {ResState, V}};
        _ -> false
      end
    end, OutEdgesMapped),

  Mapped = lists:map(
    fun ({ResState, Elem}) ->
      case lists:search(fun ({F, _}) -> F == Elem end, Offers) of
        {value, {_, NewBody}} ->
          NewCurrentStates = dict:store(From, ResState, CurrentStates),
          check_function_session(Self, {ok, NewCurrentStates}, NewBody);
        false ->
          {error, io:format("~s~w, in ~w~n", [color:red("Expected offer but did not find: "), Elem, Offers])}
      end
    end, ExpectedOffers),

  ProgressedMapped = lists:map(fun (NewCurrentStates) -> progress_states(Self, NewCurrentStates) end, Mapped),

  V = lists:all(fun (Elem) -> Elem == lists:nth(1, ProgressedMapped) end, ProgressedMapped),
  if
    V -> lists:nth(1, Mapped);
    true -> {error, io:format("~s~w~n", [color:red("All offers are not equal: "), ProgressedMapped])}
  end;
  % For each element in the tail clauses check that we hit the
  % We should after progression make sure everything is in the same state
check_function_session(_, _, {op, _, '!', Receiver, {tuple, _, [{atom, _, _}, {_, _, _}]}}) ->
  {error, io:format("~s~w~n", [color:red("The thing sent to, is required to be an atom but was: "), Receiver])};
check_function_session(_, _, {op, _, '!', _, {tuple, _, [Error, {_, _, _}]}}) ->
  {error, io:format("~s~w~n", [color:red("The message sent, must contain the receiver id as the first element: "), Error])};
check_function_session(_, _, {op, _, '!', _, {tuple, _, [_, Error]}}) ->
  {error, io:format("~s~w~n", [color:red("We only support sending primitive values: "), Error])};
check_function_session(_, _, {op, _, '!', _, Error}) ->
  {error, io:format("~s~w~n", [color:red("The thing to be sent must be a tuple: "), Error])};
check_function_session(_, _, {op, _, Error, _, _}) ->
  {error, io:format("~s~w~n", [color:red("Only the ! operator is supported: "), Error])};
check_function_session(_, CurrentStates, {call, _, Error, _}) ->
  io:fwrite("~s~w~n", [color:yellow("Function calls are currently not considered: "), Error]),
  CurrentStates;

% List of items
check_function_session(Self, CurrentStates, [StmtHd | StmtTail]) ->
  lists:foldl(
    fun (Elem, Acc) ->
      check_function_session(Self, Acc, Elem)
    end, CurrentStates, [StmtHd | StmtTail]);

% Clause
check_function_session(Self, CurrentStates, {clause, _, _, _, Body}) ->
  check_function_session(Self, CurrentStates, Body);

% If statements
check_function_session(Self, CurrentStates, {'if', _, Clauses}) ->
  Mapped = lists:map(fun ({clause, _, _, _, Body}) -> check_function_session(Self, CurrentStates, Body) end, Clauses),
  ProgressedMapped = lists:map(fun (NewCurrentStates) -> progress_states(Self, NewCurrentStates) end, Mapped),
  V = lists:all(fun (Elem) -> Elem == lists:nth(1, ProgressedMapped) end, ProgressedMapped),
  if
    V -> lists:nth(1, Mapped);
    true -> {error, io:format("~s~w~n", [color:red("All if branches are not equal: "), ProgressedMapped])}
  end;

check_function_session(Self, CurrentStates, {'case', _, _, Clauses}) ->
  Mapped = lists:map(fun ({clause, _, _, _, Body}) -> check_function_session(Self, CurrentStates, Body) end, Clauses),
  ProgressedMapped = lists:map(fun (NewCurrentStates) -> progress_states(Self, NewCurrentStates) end, Mapped),
  V = lists:all(fun (Elem) -> Elem == lists:nth(1, ProgressedMapped) end, ProgressedMapped),
  if
    V -> lists:nth(1, Mapped);
    true -> {error, io:format("~s~w~n", [color:red("All cases are not equal: "), ProgressedMapped])}
  end;
check_function_session(_, _, T) ->
  {error, io:format("~s~p~n~n", [color:red("Not matched against: "), T])}.


check_sessions({function, _, Name, 0, Body}) ->
  case ets:match(register, {Name, '$0'}) of
    [] -> continue;
    [[T]] ->
      % Create a list of things to insert into the state
      RelevantList = lists:filter(fun({{K1, _}, _}) -> K1 == T end, ets:tab2list(session_graphs)),
      MappedRelevantList = lists:map(fun({{_, K2}, _}) -> {K2, initial} end, RelevantList),

      % Create dict from list
      CurrentStates = dict:from_list(MappedRelevantList),

      io:fwrite("=== Check Session for: ~p ~p ===~n", [Name, T]),
      io:fwrite("Relevant initial states:~n", []),

      lists:foreach(fun(G) -> io:fwrite("\t~p~n", [G]) end, dict:to_list(CurrentStates)),
      %io:fwrite("===~n", []),
      %io:fwrite("~p~n", [Body]),
      %io:fwrite("======~n~n", []),

      io:fwrite("Check output:~n", []),

      case check_function_session(T, {ok, CurrentStates}, Body) of
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
            [] ->
              io:fwrite("=== ~s~w ===~n~n", [color:green("All sessions are in a final state for: "), T]),
              continue;
            B -> {error, io:format("=== ~s~w ===~n~n", [color:red("The states are not in a final state: "), B])}
          end;
        {error, Message} ->
          {error, Message}
      end


  end;
check_sessions({function, _, _, Error, _}) ->
  {error, io:format("~s~w~n", [color:red("Only functions of arity 0 are allowed: "), Error])};
check_sessions(_T) ->
  %io:fwrite("Not considered: ~p~n~n", [T]),
  continue.

do_transform({attribute, _, session, {{Key1, Key2}, Value}}) ->
  ets:insert(session, {{Key1, Key2}, Value}),
  ets:insert(session, {{Key2, Key1}, dualize(Value)}),
  io:fwrite("Session Attribute: ~w~n", [Value]),
  continue;
do_transform({attribute, _, register, Value}) ->
  ets:insert(register, Value),
  io:fwrite("Register Attribute: ~w~n", [Value]),
  continue;
do_transform(_T) ->
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


create_fsm(Graph, [eot], CurrentNode) ->
  digraph:add_vertex(Graph, CurrentNode, [final]);
create_fsm(Graph, [eot | _], CurrentNode) -> % This should be an error
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
  NewNodes = lists:map(
    fun ({Label, S}) ->
      NewNode = digraph:add_vertex(Graph),
      digraph:add_edge(Graph, CurrentNode, NewNode, [{choose, Label}]),
      create_fsm(Graph, S, NewNode)
    end, T),
  % Create a collection vertex
  NewNode2 = digraph:add_vertex(Graph),
  % Add a unlabeled edge from all new nodes to the collection vertex
  lists:foreach(
    fun (Node) ->
      digraph:add_edge(Graph, Node, NewNode2, [])
    end, NewNodes),
  create_fsm(Graph, Tail, NewNode2);

create_fsm(Graph, [{offer, T} | Tail], CurrentNode) ->
  % Create a state machine for each label
  NewNodes = lists:map(
    fun ({Label, S}) ->
      NewNode = digraph:add_vertex(Graph),
      digraph:add_edge(Graph, CurrentNode, NewNode, [{offer, Label}]),
      create_fsm(Graph, S, NewNode)
    end, T),
  % Create a collection vertex
  NewNode2 = digraph:add_vertex(Graph),
  % Add a unlabeled edge from all new nodes to the collection vertex
  lists:foreach(
    fun (Node) ->
      digraph:add_edge(Graph, Node, NewNode2, [])
    end, NewNodes),
  create_fsm(Graph, Tail, NewNode2);

create_fsm(_Graph, [], CurrentNode) ->
  CurrentNode.


progress_states(_, {error, S}) ->
  {error, S};
progress_states(Self, {ok, CurrentStates}) ->
  {ok, dict:map(
    fun (Key, CurrentState) ->
      progress_state(Self, {ok, CurrentState}, Key)
    end, CurrentStates)}.


progress_state(_, {error, S}, _) ->
  {error, S};
progress_state(Self, {ok, CurrentState}, Other) ->
  [[Graph]] = ets:match(session_graphs, {{Self, Other}, '$0'}),
  OutEdges = digraph:out_edges(Graph, CurrentState),
  OutEdgesMapped = lists:map(fun (G) -> digraph:edge(Graph, G) end, OutEdges),
  K = lists:search(
    fun(T) ->
      case T of
        {_, _, _, []} -> true;
        _ -> false
      end
    end, OutEdgesMapped),

  case K of
    {value, {_, _, ResState, _}} ->
      ResState;
    false ->
      CurrentState
  end.