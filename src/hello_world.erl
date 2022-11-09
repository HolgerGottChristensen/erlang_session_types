%%%-------------------------------------------------------------------
%%% @author holgerchristensen
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Nov 2022 00.13
%%%-------------------------------------------------------------------
-module(hello_world).
-author("holgerchristensen").

%% API
-compile([{parse_transform, test_pt}]).
-export([main/0, hello1/0, hello2/0, parse/0]).

-register([
  {id1, hello1},
  {id2, hello2}
]).

-session({{id1, id2}, [
  {send, int},
  {recv, int},
  eot
]}).

main() ->
  PID1 = spawn(?MODULE, hello1, []),
  PID2 = spawn(?MODULE, hello2, []),
  register(id1, PID1),
  register(id2, PID2).


hello1() ->
  id2 ! {id1, "Test"}.

hello2() ->
  receive {id1, Val} ->
    io:fwrite("Forms = ~p~n", [Val])
  end.

parse() ->
  {ok, AST} = epp:parse_file("src/hello_world.erl", [{includes, "include"}]),
  AST.

