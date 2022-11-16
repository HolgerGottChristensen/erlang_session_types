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
  {hello1, id1},
  {hello2, id2}
%  {hello3, id3}
]).

-session({{id1, id2}, [
  {send, integer},
  {send, integer},
  {recv, integer},
  eot
]}).

%-session({{id1, id3}, [
%  {send, integer},
%  eot
%]}).

main() ->
  PID1 = spawn(?MODULE, hello1, []),
  PID2 = spawn(?MODULE, hello2, []),
  PID3 = spawn(?MODULE, hello3, []),
  register(id1, PID1),
  register(id2, PID2),
  register(id3, PID3).


hello1() ->
  id2 ! {id1, 42},
  id2 ! {id1, 42},
  %id3 ! {id1, 42},
  receive {id2, Val} when is_integer(Val) ->
    io:fwrite("Forms = ~p~n", [Val])
  end.

hello2() ->
  receive {id1, Val} when is_integer(Val) ->
    io:fwrite("Forms = ~p~n", [Val])
  end,
  receive {id1, Val2} when is_integer(Val2) ->
    io:fwrite("Forms = ~p~n", [Val2])
  end,
  id1 ! {id2, 42}.

hello3() ->
  receive {id1, Val} when is_integer(Val) ->
    io:fwrite("Forms = ~p~n", [Val])
  end.

parse() ->
  {ok, AST} = epp:parse_file("src/hello_world.erl", [{includes, "include"}]),
  AST.

