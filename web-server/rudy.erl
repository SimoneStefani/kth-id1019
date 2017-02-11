%%%-------------------------------------------------------------------
%%% @author Simone Stefani
%%% @copyright (C) 2017, simonestefani.me
%%% Created : 11. Feb 2017 15:35
%%%-------------------------------------------------------------------
-module(rudy).

-export([start/1, stop/0]).


%%%-------------------------------------------------------------------
%%% Public API: start and stop web server at specific port
%%%-------------------------------------------------------------------

start(Port) ->
  register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
  exit(whereis(rudy), "time to die").


%%%-------------------------------------------------------------------
%%% Server implementation
%%%-------------------------------------------------------------------

% Initialize the server, takes a port number, open a listening socket 
% and passe the socket to the handler/1. Once the request has been 
% handled the socket is be closed.
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

% Listen to the socket for an incoming connection. Once a client has 
% connected it passes the connection to request/1. When the request 
% is handled the connection is closed.
handler(LSock) ->
  case gen_tcp:accept(LSock) of
    {ok, Sock} ->
      request(Sock),
      gen_tcp:close(Sock),
	    handler(LSock);
    {error, Reason} ->
      error
  end.

% Read the request from the client connection and parse it. It then 
% parses the request using the http parser and pass the request to 
% reply/1. The reply is then sent back to the client.
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

% Decide what to reply and how to turn the reply into a well formed 
% HTTP reply.
reply({{get, URI, _}, _, _}) ->
  timer:sleep(40),
  http:ok("Hello!").