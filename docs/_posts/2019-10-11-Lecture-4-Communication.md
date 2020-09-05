---
layout: post
date: 2019-10-11 15:00
categories: TeachingWeek02
---
Need communication for synchronisation and control, or to share data

## Communication in Distributed Systems
### Methods of Communication
* Shared Memory
    * Shared between threads (same address space) or methods for sharing between process
    * In a distributed system, there is no physical way to share memory (have to have some sort of message passing)
* Message Passing
    * Introduces latencies, higher chances of failure,

#### Message Passing
* Basic primitives: `send()` and `receive()`
* Connection vs. Connectionless
* Point to point vs Group
* Synchronous vs Asynchronous
* Buffered vs Unbuffered (queue or discard extra messages)
* Reliable vs Unreliable (e.g. TCP vs UDP)
* Message ordering guarantees
* Data Representation
    * Marshalling/Serialisation
    * Endianness

### Coupling
* Temporal
    * Do the sender and receiver have to be active at the same time?
* Spatial
    * Do they need to explicitly address each other?
* Semantic
    * Do they have to share knowledge of syntax and semantics?
* Platform
    * Homogeneous?

### Communication Modes
_Data- vs. Control- oriented communication_
* Data-Oriented
    * Facilitates data exchange between threads
* Control-oriented communication
    * Communication is a transfer of control
    * There is an assumption of what will happen when the communication occurs

_Synchronous vs. Asynchronous_
* Synchronous
    * Sender blocks until message received (or block until reply)
    * Sender and receiver have to be active at the same time (temporal coupling)
    E.g. client-server model (request-reply architecture)
    * This makes sense when you can't continue until you get your response
* Asynchronous
    * Sender continues execution after sending message
    * Message may be queued if receiver isn't active
    * Message may be processed later

_Transient vs. Persistent_
* Transient
    * Message discarded if it cannot be delivered immediately (tight temporal coupling)
    E.g. HTTP
* Persistent
    * Message stored (somewhere) until receiver can accept it
    E.g. email

_Provider- vs. Consumer- initiated_
* Provider-Initiated
    * Message is sent when the data is available
    E.g. Notifications
* Consumer-Initiated
    * Data is requested
    E.g. HTTP

_Direct- vs. Indirect- addressing_
* Direct Addressing
    * Message sent directly to a specific receiver (tight spatial coupling)
    E.g. HTTP
* Indirect Addressing
    * No explicit receiver specified
    E.g. broadcast or publish/subscribe

_Combinations of Approaches_
* [see notes]

## Communication Abstractions
* Higher-level APIs that abstract the simple 'message passing' communication model

### Message-Oriented Communication
* Provides traditional `send()` and `receive()` communication (asynchronous and synchronous, transient)
* Additionally:
  * Gives persistent communication (message queues)
  * Marshalling
  * Hides implementation details

**Example: MPI**

### Request-Reply Communication
* Request: a service or data
* Reply: result of executing service, or data
* Requirement: message formatting, protocol

**Example: Remote Procedural Calls**
* Replace IO-oriented message passing model with execution of a procedure call (executed remotely)
  * Synchronous
  * Message Passing details hidden from application
    * Just looks like you're executing a function
  * Procedure call parameters used to transmit data
  * Client calls local 'stub' that performs messaging/marshalling
    * Stub marshals the function call and parameters into a packed message (which is then unpacked on the server side)
      * Note: marshalled data must be pointer free: needs to flatten the list etc.
  * RPC is usually synchronous, however there are 'one way' or semi-asynchrnous implementations
    * e.g. procedure call that doesn't return anything can be called, and don't need to wait for execution to finish on the server side
    * Additionally, if a reply isn't needed immediately, you can interrupt the client with an RPC from server->client with a result (or client can poll server to see if results are ready or nog)
    * This is less transparent, but can deal with the performance issue
* Issues:
  * Expensive cost on performance to make remote calls
  * Many more errors that could occur than when executing a standard local procedure call
    * Many of these errors don't make sense if it is a local procedure call - how do you communicate these failures if you are trying to be transparent?

**Remote Method Invocation**
* Similar to RPCs, but in an object-oriented environment.
* Don't need to worry about server identity (since the remote location is encapsulated in the object).
* Can pass objects by-reference (since they are globally accessible)
* More natural resource management and error handling
* May still end up with heavy communication cost (constantly passing object reference around)

### Group-based Communication
* Sender performs a single `send()` message that is broadcast to multiple nodes
* Broadcast (everyone) and multicast (specific group)
* Uses
  * It is a natural approach to many problems in distributed systems (although it's difficult to implement well)
  * Replication of services/data
  * Service discovery
  * Event notification
* Issues:
  * Group membership
  * Ordering: Coordinate responses, ordering of received messages
  * Groups sending to groups sending to groups ...
  * Reliability: how to deal with message loss, group members offline

**Example: Gossip-based Communication**
* Relies on _epidemic behaviour_
* Process:
  * When `P` receives data item, it tries to push to arbitrary node `Q`
    *  If `Q` has not received this data, `P` keeps spreading
    * If `Q` has already received this data, `P` stops spreading the information with some probability

### Event-based Communication
* Generally associated with _publish/subscribe_ systems
* Process:
  * Sender process publishes events
  * Receiver process subscribes to events and receives those that are interesting to it
* Loose space / time coupling
  * This does make it difficult to build precicely/debug/verify correction
