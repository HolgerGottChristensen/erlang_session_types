-module(test_pt).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
  Graph = digraph:new(),
  digraph:add_vertex(Graph, initial),
  create_fsm(Graph, [
    {send, int},
    {recv, int},
    eot
  ], initial),
  %digraph:add_vertex(Graph, test),
  %digraph:add_edge(Graph, test_edge, test, test, []),
  %digraph:add_edge(Graph, test_edge2, test, test, []),
  io:fwrite("Forms = ~p~n", [Forms]),
  {_, Vertices, Edges, Neighbours, Cyclic} = Graph,
  io:fwrite("Nodes: ~n", []),
  lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(Vertices)),

  io:fwrite("~nEdges: ~n", []),
  lists:foreach(fun(G) -> io:fwrite("~p~n", [G]) end, ets:tab2list(Edges)),
  Forms.


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
create_fsm(Graph, [], CurrentNode) ->
  ok.