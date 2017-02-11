%%%-------------------------------------------------------------------
%%% @author Simone Stefani
%%% @copyright (C) 2017, simonestefani.me
%%% Created : 11. Feb 2017 15:35
%%%-------------------------------------------------------------------
-module(rudy).

-export([init/1]).

init(Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Opt) of
    {ok, LSock} ->
      handler(LSock),
      gen_tcp:close(LSock),
      ok;
    {error, Reason} ->
      error
  end.

handler(LSock) ->
  case gen_tcp:accept(LSock) of
    {ok, Sock} ->
      request(Sock),
      gen_tcp:close(Sock),
	    handler(LSock);
    {error, Reason} ->
      error
  end.

request(Socket) ->
  Recv = gen_tcp:recv(Socket, 0),
  case Recv of
    {ok, String} ->
      Request = http:parse_request(String),
      Response = reply(Request),
      gen_tcp:send(Socket, Response);
    {error, Reason} ->
      io:format("rudy: error: ~w~n", [Reason])
  end,
  gen_tcp:close(Socket).

reply({{get, URI, _}, _, _}) ->
  timer:sleep(40),
  http:ok("Hello!").