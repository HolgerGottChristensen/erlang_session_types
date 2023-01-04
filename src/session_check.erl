-module(session_check).

-export([parse_transform/2]).


parse_transform(Forms, _Options) ->
  io:fwrite("~n~s: ~p~n", [color:blue("Performing type check of session types..."), self()]),

  case ets:whereis(tables) of
    undefined -> ets:new(tables, [named_table, public]);
    _ -> ok
  end,

  ets:insert(tables, {{register, self()}, ets:new(register, [])}),
  ets:insert(tables, {{session, self()}, ets:new(session, [])}),
  ets:insert(tables, {{session_graphs, self()}, ets:new(session_graphs, [])}),
  ets:insert(tables, {{current_session, self()}, ets:new(current_session, [])}),

  parse_trans:plain_transform(fun do_transform/1, Forms),
  io:fwrite("~s~n~n", [color:blue("Done collecting session and register attributes. Building graphs...")]),

  lists:foreach(
    fun({K, V}) ->
      Graph = digraph:new(),
      digraph:add_vertex(Graph, initial),
      create_fsm(Graph, V, initial),
      simplify_fsm(Graph),
      ets:insert(table(session_graphs), {K, Graph})
    end, ets:tab2list(table(session))
  ),

  % Print all graphs
  lists:foreach(
    fun({K, Graph}) ->
      io:fwrite("==== Graph: ~p ====~n", [K]),
      {_, Vertices, Edges, _Neighbours, _Cyclic} = Graph,
      io:fwrite("Nodes: ~n", []),
      % Print all nodes in the current graph
      lists:foreach(
        fun(G) ->
          case G of
            {[_ | Node], Label} -> io:fwrite("\tn~p ~p~n", [Node, Label]);
            {Node, Label} -> io:fwrite("\t~p ~p~n", [Node, Label])
          end
        end, ets:tab2list(Vertices)),

      io:fwrite("Edges: ~n", []),
      % Print all edges in the current graph
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
    end, ets:tab2list(table(session_graphs))
  ),

  io:fwrite("~s~n~n", [color:blue("Graphs built. Performing session checks...")]),


  parse_trans:plain_transform(fun check_sessions/1, Forms),
  io:fwrite("~s~n~n", [color:greenb("Session types type checks!")]),

  Forms.


table(Atom) ->
  [[Res]] = ets:match(tables, {{Atom, self()}, '$0'}),
  Res.


check_function_session(_, _, {error, S}, _) ->
  {error, S};

check_function_session(Self, _, {ok, CurrentStates}, {op, _, '!', {atom, _, To}, {tuple, _, [{atom, _, Self}, {Ty, _, V}]}}) ->
  [[Graph]] = ets:match(table(session_graphs), {{Self, To}, '$0'}),
  {ok, CurrentState} = dict:find(To, CurrentStates),

  % Check the type of the send matches with the session.

  OutEdges = digraph:out_edges(Graph, CurrentState),
  OutEdgesMapped = lists:map(fun(G) -> digraph:edge(Graph, G) end, OutEdges),

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
      NewCurrentStates = dict:store(To, ResState, CurrentStates),
      {ok, NewCurrentStates};
    false ->
      Possible = lists:map(fun({_, _, _, [B]}) -> B end, OutEdgesMapped),
      {error, io:format("~s~w~s~w~n", [color:red("Could not find a suitable send edge in the fsm with type: "), Ty, color:red(". Expected one of: "), Possible])}
  end;

% Receiving messages
check_function_session(Self, FnName, {ok, CurrentStates}, {'receive', _, [{clause, _, [{tuple, _, [{atom, _, From}, {var, _, Val}]}], [[{call, _, {atom, _, TyFun}, [{var, _, Val}]}]], Body}]}) ->
  [[Graph]] = ets:match(table(session_graphs), {{Self, From}, '$0'}),
  {ok, CurrentState} = dict:find(From, CurrentStates),

  % Check the type of the send matches with the session.

  OutEdges = digraph:out_edges(Graph, CurrentState),
  OutEdgesMapped = lists:map(fun(G) -> digraph:edge(Graph, G) end, OutEdges),

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
      NewCurrentStates = dict:store(From, ResState, CurrentStates),
      NewCurrentStates2 = check_function_session(Self, FnName, {ok, NewCurrentStates}, Body),
      NewCurrentStates2;
    false ->
      Possible = lists:map(fun({_, _, _, [B]}) -> B end, OutEdgesMapped),
      {error, io:format("~sw~s~w~n", [color:red("Could not find a suitable recv edge in the fsm with type: "), Ty, color:red(". Expected one of: "), Possible])}
  end;
% Receiving offers
check_function_session(Self, FnName, {ok, CurrentStates}, {'receive', _, [{clause, _, [{tuple, _, [{atom, _, From}, {atom, _, Offer}]}], _, Body} | TailClauses]}) ->
  % Check if the clause thing matches
  [[Graph]] = ets:match(table(session_graphs), {{Self, From}, '$0'}),
  {ok, CurrentState} = dict:find(From, CurrentStates),

  % Check the type of the send matches with the session.
  OutEdges = digraph:out_edges(Graph, CurrentState),
  OutEdgesMapped = lists:map(fun(G) -> digraph:edge(Graph, G) end, OutEdges),

  Offers = [{Offer, Body}] ++ lists:map(
    fun ({clause, _, [{tuple, _, [{atom, _, _}, {atom, _, Offer2}]}], _, Body2}) ->
      {Offer2, Body2}
    end, TailClauses),

  ExpectedOffers = lists:filtermap(
    fun(T) ->
      case T of
        {_, _, ResState, [{offer, V}]} -> {true, {ResState, V}};
        _ -> false
      end
    end, OutEdgesMapped),

  Mapped = lists:map(
    fun({ResState, Elem}) ->
      case lists:search(fun({F, _}) -> F == Elem end, Offers) of
        {value, {_, NewBody}} ->
          NewCurrentStates = dict:store(From, ResState, CurrentStates),
          check_function_session(Self, FnName, {ok, NewCurrentStates}, NewBody);
        false ->
          {error, io:format("~s~w, in ~w~n", [color:red("Expected offer but did not find: "), Elem, Offers])}
      end
    end, ExpectedOffers),

  V = lists:all(fun(Elem) -> Elem == lists:nth(1, Mapped) end, Mapped),
  if
    V -> lists:nth(1, Mapped);
    true -> {error, io:format("~s~w~n", [color:red("All offers are not equal: "), Mapped])}
  end;
check_function_session(_, FnName, {ok, CurrentStates}, {call, _, {atom, _, FnName}, _}) ->
  List = dict:to_list(CurrentStates),
  AllInitial = lists:all(
    fun (CurrentState) ->
      case CurrentState of
        {_, initial} -> true;
        _ -> false
      end
    end, List),
  case AllInitial of
    true -> {ok, CurrentStates};
    false -> {error, io:format("~s~w~n", [color:red("Recursive calls should only be made when all states are initial: "), CurrentStates])}
  end;
% For each element in the tail clauses check that we hit the
% We should after progression make sure everything is in the same state
check_function_session(_, _, _, {op, _, '!', Receiver, {tuple, _, [{atom, _, _}, {_, _, _}]}}) ->
  {error, io:format("~s~w~n", [color:red("The thing sent to, is required to be an atom but was: "), Receiver])};
check_function_session(_, _, _, {op, _, '!', _, {tuple, _, [Error, {_, _, _}]}}) ->
  {error, io:format("~s~w~n", [color:red("The message sent, must contain the receiver id as the first element: "), Error])};
check_function_session(_, _, _, {op, _, '!', _, {tuple, _, [_, Error]}}) ->
  {error, io:format("~s~w~n", [color:red("We only support sending primitive values: "), Error])};
check_function_session(_, _, _, {op, _, '!', _, Error}) ->
  {error, io:format("~s~w~n", [color:red("The thing to be sent must be a tuple: "), Error])};
check_function_session(_, _, _, {op, _, Error, _, _}) ->
  {error, io:format("~s~w~n", [color:red("Only the ! operator is supported: "), Error])};
check_function_session(_, _, CurrentStates, {call, _, Error, _}) ->
  io:fwrite("~s~w~n", [color:yellow("Function calls are currently not considered: "), Error]),
  CurrentStates;

% List of statements
check_function_session(Self, FnName, CurrentStates, [StmtHd | StmtTail]) ->
  lists:foldl(
    fun(Elem, Acc) ->
      check_function_session(Self, FnName, Acc, Elem)
    end, CurrentStates, [StmtHd | StmtTail]);

% Clause
check_function_session(Self, FnName, CurrentStates, {clause, _, _, _, Body}) ->
  check_function_session(Self, FnName, CurrentStates, Body);

% If statements
check_function_session(Self, FnName, CurrentStates, {'if', _, Clauses}) ->
  % Call all cases with the current state as the input and collected the resulting states.
  Mapped = lists:map(fun({clause, _, _, _, Body}) -> check_function_session(Self, FnName, CurrentStates, Body) end, Clauses),
  % Check that all elements in the mapped list are equal
  V = lists:all(fun(Elem) -> Elem == lists:nth(1, Mapped) end, Mapped),
  % If they are all equal, return the element.
  if
    V -> lists:nth(1, Mapped);
    true -> {error, io:format("~s~w~n", [color:red("All if branches are not equal: "), Mapped])}
  end;

check_function_session(Self, FnName, CurrentStates, {'case', _, _, Clauses}) ->
  % Call all cases with the current state as the input and collected the resulting states.
  Mapped = lists:map(fun({clause, _, _, _, Body}) -> check_function_session(Self, FnName, CurrentStates, Body) end, Clauses),
  % Check that all elements in the mapped list are equal
  V = lists:all(fun(Elem) -> Elem == lists:nth(1, Mapped) end, Mapped),
  % If they are all equal, return the element.
  if
    V -> lists:nth(1, Mapped);
    true -> {error, io:format("~s~w~n", [color:red("All cases are not equal: "), Mapped])}
  end;
check_function_session(_, _, _, T) ->
  {error, io:format("~s~p~n~n", [color:red("Not matched against: "), T])}.


check_sessions({function, _, Name, 0, Body}) ->
  case ets:match(table(register), {Name, '$0'}) of
    [] -> continue;
    [[T]] ->
      % Create a list of things to insert into the state
      RelevantList = lists:filter(fun({{K1, _}, _}) -> K1 == T end, ets:tab2list(table(session_graphs))),
      MappedRelevantList = lists:map(fun({{_, K2}, _}) -> {K2, initial} end, RelevantList),

      % Create dict from list
      CurrentStates = dict:from_list(MappedRelevantList),

      io:fwrite("=== Check Session for: ~p ~p ===~n", [Name, T]),
      io:fwrite("Relevant initial states:~n", []),

      lists:foreach(fun(G) -> io:fwrite("\t~p~n", [G]) end, dict:to_list(CurrentStates)),

      io:fwrite("Check output:~n", []),

      case check_function_session(T, Name, {ok, CurrentStates}, Body) of
        {ok, FinalStates} ->
          % Check that all current states are in a final state
          InFinal = lists:filter(
            fun({To, CurrentState}) ->
              [[Graph]] = ets:match(table(session_graphs), {{T, To}, '$0'}),
              case digraph:vertex(Graph, CurrentState) of
                {_, [final]} -> false;
                {_, [goto_final]} -> false;
                _ -> true
              end
            end, dict:to_list(FinalStates)),

          case InFinal of
            [] ->
              io:fwrite("=== ~s~w ===~n~n", [color:green("All sessions are in a final state for: "), T]),
              continue;
            B ->
              io:fwrite("=== ~s~w ===~n~n", [color:yellow("Some states are not in a final state, but are still valid: "), B]),
              continue
          end;
        {error, Message} ->
          {error, Message}
      end


  end;
check_sessions({function, _, _, Error, _}) ->
  {error, io:format("~s~w~n", [color:red("Only functions of arity 0 are allowed: "), Error])};
check_sessions(_T) ->
  continue.


do_transform({attribute, _, session, {{Key1, Key2}, Value}}) ->
  ets:insert(table(session), {{Key1, Key2}, Value}),
  ets:insert(table(session), {{Key2, Key1}, dualize(Value)}),
  io:fwrite("Session Attribute: ~w~n", [Value]),
  continue;
do_transform({attribute, _, register, Value}) ->
  ets:insert(table(register), Value),
  io:fwrite("Register Attribute: ~w~n", [Value]),
  continue;
do_transform(_T) ->
  continue.


dualize(SessionType) ->
  lists:map(
    fun(T) ->
      case T of
        {send, U} -> {recv, U};
        {label, U} -> {label, U};
        {goto, U} -> {goto, U};
        {recv, U} -> {send, U};
        {offer, U} -> {choose, lists:map(fun({Label, S}) -> {Label, dualize(S)} end, U)};
        {choose, U} -> {offer, lists:map(fun({Label, S}) -> {Label, dualize(S)} end, U)};
        eot -> eot
      end
    end, SessionType).


current_labels({[_ | _], Label}) ->
  Label;
current_labels({_, Label}) ->
  Label.


node_id_with_label(Graph, Lab) ->
  {_, Vertices, _Edges, _Neighbours, _Cyclic} = Graph,
  Id = lists:search(
    fun({_, Label}) ->
      lists:search(fun (A) -> A == {label, Lab} end, Label) /= false
    end, ets:tab2list(Vertices)),
  Id.


create_fsm(Graph, [eot], CurrentNode) ->
  Label = current_labels(digraph:vertex(Graph, CurrentNode)),
  digraph:add_vertex(Graph, CurrentNode, [final | Label]);

create_fsm(Graph, [{label, T} | Tail], CurrentNode) ->
  Label = current_labels(digraph:vertex(Graph, CurrentNode)),
  digraph:add_vertex(Graph, CurrentNode, [{label, T} | Label]),
  create_fsm(Graph, Tail, CurrentNode);

create_fsm(Graph, [{goto, T}], CurrentNode) ->
  {value, {EndNode, _}} = node_id_with_label(Graph, T),
  digraph:add_edge(Graph, CurrentNode, EndNode, []);

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
    fun({Label, S}) ->
      NewNode = digraph:add_vertex(Graph),
      digraph:add_edge(Graph, CurrentNode, NewNode, [{choose, Label}]),
      create_fsm(Graph, S, NewNode)
    end, T),
  % Create a collection vertex
  NewNode2 = digraph:add_vertex(Graph),
  transfer_same_labels(Graph, NewNodes, NewNode2),
  % Add a unlabeled edge from all new nodes to the collection vertex
  lists:foreach(
    fun(Node) ->
      digraph:add_edge(Graph, Node, NewNode2, [])
    end, NewNodes),
  create_fsm(Graph, Tail, NewNode2);

create_fsm(Graph, [{offer, T} | Tail], CurrentNode) ->
  % Create a state machine for each label
  NewNodes = lists:map(
    fun({Label, S}) ->
      NewNode = digraph:add_vertex(Graph),
      digraph:add_edge(Graph, CurrentNode, NewNode, [{offer, Label}]),
      create_fsm(Graph, S, NewNode)
    end, T),
  % Create a collection vertex
  NewNode2 = digraph:add_vertex(Graph),
  transfer_same_labels(Graph, NewNodes, NewNode2),
  % Add a unlabeled edge from all new nodes to the collection vertex
  lists:foreach(
    fun(Node) ->
      digraph:add_edge(Graph, Node, NewNode2, [])
    end, NewNodes),
  create_fsm(Graph, Tail, NewNode2);

create_fsm(_Graph, [], CurrentNode) ->
  CurrentNode.

% Iterate through each node
% If the first outgoing edge is empty, move all ingoing edges to point to the resulting node.
simplify_fsm(Graph) ->
  Nodes = digraph:vertices(Graph),
  EdgesAndToAndNode = lists:filtermap(
    fun (Node) ->
      EdgeIds = digraph:out_edges(Graph, Node),
      Edges = lists:map(fun (EdgeId) -> digraph:edge(Graph, EdgeId) end, EdgeIds),
      case Edges of
        [{_, _, To, []}] -> {true, {lists:map(fun (EdgeId) -> digraph:edge(Graph, EdgeId) end, digraph:in_edges(Graph, Node)), To, Node}};
        _ -> false
      end
    end, Nodes),

  lists:foreach(
    fun ({Edges, To, Node}) ->
      lists:foreach(
        fun ({_, From, _, Labels}) ->
          digraph:add_edge(Graph, From, To, Labels)
        end, Edges),
      digraph:del_vertex(Graph, Node)
    end, EdgesAndToAndNode).


labels(Graph, Node) ->
  {_, Labels} = digraph:vertex(Graph, Node),
  Labels.


transfer_same_labels(Graph, SourceNodes, Target) ->
  SourceNodesLabels = lists:map(fun (SourceNode) -> labels(Graph, SourceNode) end, SourceNodes),
  AllSame = lists:all(fun(Elem) -> Elem == lists:nth(1, SourceNodesLabels) end, SourceNodesLabels),
  if
    AllSame -> digraph:add_vertex(Graph, Target, lists:nth(1, SourceNodesLabels));
    true -> []
  end.
