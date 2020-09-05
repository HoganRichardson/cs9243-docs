---
layout: post
date: 2019-11-21 11:00
categories: TeachingWeek08
---

### Question 1
_In an interview about designing distributed systems Ken Arnold (the original lead architect of JavaSpaces) said "State is hell. You need to design systems under the assumption that state is hell. Everything that can be stateless should be stateless". Why does state pose a problem in distributed systems?_

**Unprepared Answer**: A global state requires all nodes to agree on a centralised world view. This means there must be some sort of synchronisation to ensure that every node is always working on the same global state. This poses problems if crashes occur during the synchronising or communication about changes to the state fail. Since distributed systems try to have no centralisation by definition, maintaining a global state is challenging, and consumes CPU and communication resources to try and synchronise states. If the DS can work stateless, then there is no need for nodes to be aware of a global state or the state of other nodes, making the distributed system more adaptable, and making failures (comms failure, nodes crashing) easier to deal with.

* **Sample Answer**: (✔️ Synchronisation), (✔️ Fault Tolerance), 
	* Replication: Have to keep replicas consistent
	* Fault Tolerance: 
		* If a server stores state for client, then client has to explicitely close sessionso server can free resources (if client crashes) server can waste resources. 
		* If stateful server crashes, needs to recover state
	* Mitiation: client can't migrate to another serer without transfering state to server (servers may need to synchronise state transfer themselves)
	* Security: Extra trust in server storing client's information

### Question 2
_SOAP and XML-RPC are two examples of RPC protocols that use XML as their data format and HTTP as their transport. Discuss the benefits and drawbacks of using XML as a data format. Also discuss the benefits and drawbacks of using HTTP as a transport._

**Unprepared Answer**: XML is (by definition) an "extensible" format. Converting data to and from XML through serialisation (and deserialisation) is a straightforward process, and allows for easy communication of unserialised data. XML, however, is a text-based format and thus the serialised data will be larger (??). HTTP is a stateless protocol, which makes it good for independent systems, however it does not have (built in) a means of ensuring reliable data transfer. Being stateless does mean that more bandwidth is used during communications, as endpoints do not retain states. However in an RPC implementation, the remote call can be made and the result returned in a fairly simpel manner (especially when coupled with the XML data format), making this a suitable implementation for distributed systems such as RPC.
The common format of XML means that there is no issues with compatibility or representations of data on the endpoints, since they can serialise/unserialise accordingly. 

* **Sample Answer**:(✔️  "Marshalling" and "Unmarshalling") (✔️  Size overhead)
	* XML Benefits:
		* XML Is standardised (standard libraries help programming middleware)
		* XML is text-based (easy to debug)
	* XML Drawacks:
		* Encoding adds overhead (marshalling, re-encoding binary data)
		* Text based (so binary data has to be encoded e.g. base64), increases sise, causes loss in performance
	* HTTP Benefits:
		* Widely-used standard (many libraries)
		* Integrates with existing web servers
		* Text-based (easy to debug)
		* Traffic is usually allowed through firewalls
	* HTTP Drawacks:
		* Not optimised for RPC
		* Text-based causes extra oerhead
		* Server-initiated data flow is difficult (for callbcaks or delayed asynchronous) - need to start server on client or something

### Question 3
_Servers hosting popular Web sites often receive more requests than they can handle, causing their performance to suffer. A typical way of overcoming this problem is to to replicate the contents on other servers. Describe some of the problems that this may introduce. Discuss possible solutions to the problems you mention._

**Unprepared Answer**: 
* Replication Consistency: maintaining up-to-date copies of the data when it is modified on one replica. 
	* Possible Solution: using write-invalidate messages to invalidate cached data on other replicas to be marked as out of date when a write occurs
* Concurrent Writes: If two replicas have the same data modified at the same time, how does the system know which version should be the latest?
	* Possible Solution 1: Take most recent. This could result in previous modifications being lost. Also the question of how to determine most recent (need synchronised clocks)
	* Possible Solution 2: log all changes to files, and replay these - still need some way of ensuring message ordering when communicating changes
	* Possible Solution 3: make all write requests go to one centralised write server - this results in scalability problem again as write server may become overloaded

* **Sample Answer**: (✔️  Consistency)
	* Problems
		1. Consistency
		2. Redirect clients to appropriate replicas
		3. Difficulty of replicating dynamic content
		4. Deciding on replica locations
	* Solutions 
		1. Push updates/invalidations to replicas. Use weakened consistency, and give TTL
		2. Use rediector server, or allow DNS round robin between replicas
		3. Only replicate static (immutable) data, and do dynamic generation server side, or replicate the underlying database as well
		4. Guess where requests will come from (based on where expected requests will come from). Monitor usage of replicas (or look at past traffic), or place at network endpoints.

### Question 4
_Read your writes is a client-centric consistency model:_
* _1. Describe this model_
	* **Unprepared Answer**: This model ensures that for any write on data object `x`, a successive read on `x` will always show the latest write
	* **Sample Answer**: ✔️

* _2. A naive implementation of this model requires that each replica server maintain a write set of the write operations that it has seen. Likewise, each client maintains a read set of the write operations that it has seen. For this implemenatation we assume that each write operation is identified by a unique identifier (note that because we are dealing with replication a write operation will be executed at each replica server. Each replicated execution of the same write operation will have the same identifier). This identifier is generated by the server that accepts the operation for the first time (i.e., the server where the write is initiated). Complete (and describe) the design of this naive implementation of read your writes. Provide an example of how your design works._
	* **Unprepared Answer**: (complete and describe)(??) ❌
	* **Sample Answer**: 
		* Server (write initiation server) assigns unique ID to write
		* Client that performs write stores it in it's ID set
		* When client performs read, it sends it's write set to server first. Server must ensure that it has completed the write history of the client before returning result
			* Server could either block and wait to receive write, request writes from other replica, or return read failure to the client
		* N.B. servers also maintain their write set
		* [Detailed example with multiple cases]

* _3. A problem with the naive implementation introduced above is that the write and read sets may become very large, leading to poor performance. A more efficient solution is possible using vector timestamps. Sketch the design of a read your writes implementation that avoids the overhead of large read and write sets by using vector timestamps. Provide an example of how your design works._
	* **Unprepared Answer**: Each replica (client??) maintains a vector of all timestamps (i.e. the latest timestamp of data object `x` at each replica). The replicas first synchronise this timestamp list, by adding their timestamp and sending the vector (broadcast). All other nodes update their vectors with this timestamp, and broadcast their vectors. (broadcast/multicast??). 
	* **Sample Answer**: 
		* Each write receives timestamp from initiating server (local timestamps)
		* Client keeps track of timestamps of last write performed on each server (in the vector)
		* Each server keeps timestamp from last write it received from another server
		* When read occurs, client passes vector to server. Server must ensure that the client vector < server's write vector before it can continue
		* [Another detailed example with multiple cases]

### Question 5
_The [Erlang client server exercise](https://www.cse.unsw.edu.au/~cs9243/19t3/exercises/client-server.html) could also be an exam question._

**Unprepared Answer**
```erl
module(client).
export([send/0, receive/0]).

send() ->
	Server ! {msg, self(), "Hello World"},
	get_response(),
	end.

get_response() ->
	receive
		{response, From, Payload} ->
			parseResponse(),
	end.

server() ->
	get_msgs() -> % Main server loop
		receive 
			{msg, From, Payload} ->
				dealwithmsg(),
				get_msgs();
			stop ->
				ok,
		end.
```

**Correct Solution**
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

### Comments
> These are examples of the kinds of questions you may encounter. This is an open book exam so it requires you to understand the material. A result of this is that you may be required to combine concepts and ideas that were presented separately (in separate lectures). Be prepared to think, not regurgitate.

> When answering the questions answer briefly, overly lengthy answers will be penalised. Make sure that you answer only the question that has been asked (but make sure that you answer all the questions you have been asked). Also make sure that you don't contradict yourself. If you give a correct answer and then contradict yourself by giving the wrong answer too, then the answer won't count as being right. You may draw diagrams to clarify your answer, but make sure that you explain the diagrams. Diagrams generally don't speak for themselves.

