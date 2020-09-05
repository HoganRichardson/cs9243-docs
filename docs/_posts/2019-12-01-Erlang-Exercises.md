---
layout: post
categories: Revision
---

## Client-Server

```erl
-module(clientserver).

% Client code using increment server
client (Server) ->
	Server ! {self (), 10},
	receive
		{From, Reply} -> io:format("Result: ~w~n", [Reply])
	end.

% Server loop for increment server
loop () ->
	receive
		{From, Msg} -> From ! {self(), Msg+1},
		   loop();
		stop -> true
	end.

% Start Server
start_server() -> spawn (fun() -> loop() end).
```

## Erlang RPC
```erl
-module(rpc).

% Client code
client (Server) ->
	register(server, Server),
	Result = inc (10),
	io:format ("Result: ~w~n", [Result]).

% Client Stub for 'increment'
inc (Value) ->
	% Any Marshalling would be done here
	server ! {self(), inc, Value},
	receive
		{From, inc, Reply} -> Reply
	end.

% Server code: increment Implementation
inc (Value) -> Value + 1.

% Server Dispatch Loop
server() ->
	receive
		{From, inc, Value} ->
			From ! {self(), inc, inc(Value)}
	end,
	server().
```
