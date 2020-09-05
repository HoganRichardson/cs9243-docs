---
layout: post
categories: TeachingWeek04
---

## Distributed Algorithms
>Algorithms that are intended to work in a distributed environment (runs on multiple machines at the same time to solve the problem)

Accomplish tasks such as:
* Communication
* Accessing resources (e.g. replicated data)
* Allocating resources, consensus (agreement on resources etc.)

You achieve _synchronisation & coordination_ through distributed algorithms, and distributed algorithms usually re3quire some sort of _synchronisation & coordination_!

### Timing Models of a Distributed System
* Timing models affected by execution speed, communication delay, clocks and clock drift.
* Most real distributed systems are a hybrid of synchronous/asynchronous

**Synchronous Distributed System**
> Time Variance is bounded

* Bounded execution speed and time
* Communication: bounded transmission delay
* Bounded clock drift

Can rely on timeouts to detect failure (can make assumptions about state of machines given the time)

- [x] Easier to design distributed algorithms
- [ ] Very restrictive requirements
    * Limit concurrent processes per processor (pre-emption)

**Asynchronous Distributed System**
> Time Variance is _not_ bounded

* Different execution steps can have varying duration
* Communication transmission delays vary widely
* Arbitrary clock drift

Allows no assumption about time intervals
- [ ] Can't rely on timeouts to detect failure
- [ ] Most asynchronous problems are hard to solve
- [x] Solution for asynchronous system will also work in synchronous system.

### Evaluating Distributed Algorithms
**Key properties**
* Safety: _nothing bad happens_
* Liveness: _something good eventually happens_

**General Properties**
* Performance (many different metrics: number of messages exchanged, response/wait time, delay/throughput, complexity)
    * Throughput = `1 / (delay + executiontime)`
* Efficiency (resource usage: memory, CPU etc.)
* Scalability
* Reliability (number of points of failure)

## Synchronisation & Coordination
> Doing the right thing (coordination) at the right time (synchronisation)

**Coordination**
* Coordination Actions:
    * What actions will occur?
    * Who will perform the actions?
* Agree on Values:
    * Agree on global value
    * Agree on environment
    * Agree on state (is the system in state of error? etc.)

**Synchronisation**
* Order of all actions:
    * Total ordering of events, instructions, communication
    * Ordering of access to resources (e.g. mutex)

(Requires some concept of time)

**Main Issues**
* Time and Clocks
* Global State
* Concurrency Control

### Time & Clocks
* Notion of _Global Time_
    * _"Absolute"_ time (conceptually this works, but of course time is relative)
    * UTC (Coordinated Universal Time)
        * Leap seconds added to atomic time to ensure it still lines up with astronomical time (i.e. still daytime when sun shines!)
* _Local Time_: not synchronised to a global source
* Clocks in Computers:
    * Timestamps to denote the time a certain event occurred
    * Synchronisation using clocks:
        * perform events at exact times
        * Logging of events (can order events by timestamp)
        * Tracking (e.g. tracking object with multiple cameras)
        * `make` (examine source timestamp vs object timestamp - do we need to recompile? If building on another computer, if clocks aren't synchronised, may have issues).
        * Ordering of messages
* _Physical Clocks_
    * Based on actual time
    * Ideally `C`<sub>`p`</sub>`(t) = t`, but clocks _drift_ so must regularly synchronise with UTC.
        * Skew: instantaneous difference
        * Drift: rate of change of skew
    * Synchronising:
        * Internal Synchronisation: only synchronise with each other
        * External Synchronisation: clocks synchronise to an external time source (time server: server that has/calculates correct time)

#### Clock Synchronisation Approaches
**Berkeley Algorithm**
_Internal,  accuracy 20-25 ms_
* Central time daemon - periodically collects time from all nodes, average the result, and then send out a time change to each node to make them in sync
    * Sends/receives relative offsets to each nodes (never needs to deal with actual time)
* Generally useful in local area network, or when you don't need to know what global time is.
* There is no central source of truth, so no notion of global time

**Cristian's Algorithm**
_Has UTC receiver, passive, accuracy 1-10ms (The RTT in LAN)_
* Clients periodically request the time
    * Clients can't set the time backwards (it's usually difficult to deal with events from the future - e.g. timestamps in the future)
    * If time server is behind your clock, you slow client clock down until it catches up to time server - this way time doesn't go backwards!
* Take propagation and interrupt handling delay into account (calculate difference, or take series of measurements and average delay, or use msg with shortest delay)
- [ ] Not scalable: centralised server

**Network Time Protocol (NTP)**
_Accuracy 1-50 ms_
* Hierarchy of servers:
    * Primary server has UTC clock
    * Secondary server: connects to primary
    * ...
* Methods
    * Multicast (LAN, low accuracy)
    * Procedure Call (clients poll)
    * Symmetric (between peer servers at same level)
* Synchronisation
    * Estimate clock offsets and tx delays between two nodes (retain past estimates, and choose offset estimate for lowest tx delay)
    * Also determines unreliable servers

#### Logical Clocks
Event ordering is more important than physical time:
* Events in a single process are ordered
* Processes need to agree on ordering of causally-related events

**Local Ordering**
* e -> <sub>i</sub> e'
* Everything that happens in the same process is causally related

**Global Ordering**
* Lamport's global _happened before_ relation _->_
* Smallest relation such that:
    1. e -><sub>i</sub> e' implies e->e' (<sub>i</sub> is local)
    2. For every message, send(m)->recv(m)
    3. Transitivity: e->e' and e'->e'' implies e->e''
* The relation `->` is a partial order:
    * If a->b, then a causally affects b
    * Unordered events are considered to be concurrent (`||`)

_Implementation_
* Software counter locally compute the happened-before relation
* Each process maintains a logical clock
* Process:
    1. Before timestamping a local event, increment local clock
    2. Whenever a message is sent between processes:
        * Increment local logical clock, and send new value with message
        * Receiver takes max of current local clock and received clock, and uses max (increments it)
* Properties:
    * `a->b` implies `L(a) < L(b)`
    * `L(a) < L(b)` does not necessarily imply `a->b`
* Total event ordering:
    * Lexicographical ordering

**Vector Clocks**
* In Lamport local clocks, `L(a) < L(b)` does not necessarily imply `a->b`
    * So can't deduce causal dependencies from timestamps
    * Because clocks advance independently (or via messages) - there is no historic information on where advances come from)

Vector Clocks:
* At each process, maintain a clock for every other process
* Events are timestamped with a vector `V` which is a vector of the knowledge about each other clock
    * i.e. `V`<sub>`i`</sub>`[j]` is i's knowledge about j's clock
* Process:
    1. Initially, V = 0
    2. Before _p<sub>i</sub>_ timestamps an event, increment's it's own record.
    3. Whenever a message is sent between processes:
        * Process increment's its clock value and sends entire vector
        * Receiver merges the vector with it's local vector
            * Takes the max of each element, and increments it's own record

### Global State
Determining Global Properties:
* Distributed garbage collection: do any references still exist to an object?
    * There could be a message in transmission that references the object
* Distributed deadlock detection
* Distributed termination detection: did a set of processes cease activity
    * Or are they just waiting for messages?
* Distributed checkpoint: what is a correct state of the system to save?

#### Consistent Cuts
* Need to combine information from multiple nodes.
* Without global time, how do we know whether local information is consistent?
* Local state sampled at arbitrary points is not consistent: what is the criteria for 'consistent global state'?
* A _cut_ is a history up to a certain event.
    * Consistent Cut: For all events e', if e->e', then e is in the cut
* _Frontier_ is the final events in a cut
* A global state is consistent if it corresponds to a consistent cut (a global history is a sequence of consistent global states)

**Chandy and Lamport's Snapshots**

Determines a consistent global state, takes care of messages in transit (don't need to 'stop the world'). Useful for evaluating stable global properties

_Assumptions:_
* Reliable communication and failure-free processes
* Point-to-point message delivery is ordered
* Process/channel graph must be strongly connected
* On termination:
    * Processes hold their own local state components
    * Nodes hold a set of messages in transit during the snapshot

Process:
1. One process initiates algorithm by sending a marker message `*` (and recording it's local state
2. For each node, when it receives `*`:
    * Save local message, and send marker messages to all connections
    * Need to store messages up until marker received on that channel
3. Local contribution complete after markers received on all incoming channels

#### Spanner & TrueTime
Global Distributed Database desires:
* External consistency (linearisability)
* Lock-free read transactions (scalability)

**External Consistency with Global Clocks**
* Data versioned using timestamp
* Read operations performed on a snapshot (provides lock-less reads - faster)
* Write operation has unique timestamp
    * Write operations are protected by locks, and get global time during transaction
* Problem: it's impossible to get perfectly-synchronised clocks (so timestamps could overlap!)

**TrueTime**
* Add uncertainty to timestamps:
    * For every timestamp, there is:
        * the value of the time stamp
        * Earliest and latest values (these vals are maximum skew from actual time in either direction)
* Add delay to transaction:
    * To ensure that timestamps can't overlap
    * Wait until `tt.now(earliest) > s.latest`
* Needs to have _'reasonably synchronised'_ clocks

### Concurrency Control
* Concurrency in Distributed Systems introduces more challenges on the typical concurrency problems
    * No direct shared resources (e.g. memory)
    * No global state, no global clock
    * No centralised algorithms
    * More concurrency

#### Distributed Mutual Exclusion
Concurrent access to distributed resources - must prevent race conditions during critical regions
1. _Safety_: at most one process can execute critical section at a time
2. _Liveness_: requests to enter/exit critical region eventually succeed
3. _Ordering_: (not critical) requests are processed in happened-before ordering
* Note: Evaluation metrics of Distributed Algorithms (see above)

**Central Server**
_3 messages exchanged, delay: 2 messages, reliability is OK_
* Requests to enter/exit critical region are sent to a _lock server_
    * Permission to enter is granted by receiving a token.
    * Return token to server when finished critical section
- [x] Easy to implement
- [ ] Doesn't scale well (centralised server overloading)
- [ ] Failure: central point of failure (server), or the client with token goes down

**Token Ring**
_messages exchanged: varies (if no-one is using it, token just keeps going round), delay=N/2 (max is num nodes), poor reliabilty_
* All processes are organised in logical ring
* A token is forwarded along the ring
    * Before entering critical section, process has to wait until the token arrives
    * Retain the token until left the critical section
- [ ] Ring imposes avg delay N/2 (limited scalability)
- [ ] Token messages consume bandwidth
- [ ] If any node goes down, ring is broken

**Multicasts & Logical Clocks**
_Messages exchanged: 2*(N-1), delay 2*(N-1), poor reliable_
* Processes maintain Lamport clock and can communicate pairwise
* Three states:
    * Released
    * Wanted
    * Held
* Behaviour:
    1. If process wants to enter critical section, it multicasts message, and waits until it receives a reply from every process
        2. If a process is _Released_, it immediately replies to any request to enter critical section
        3. If a process is _Held_, it delays replying until it has left critical section
        4. If a process is _Wanted_, it replies to request immediately if the requesting timestamp is jsmaller than it's own
- [ ] Multicast leads to increased overheads
- [ ] Susceptible to faults (any node crashing will cause this to fail)

### Transactions
An upgraded version of mutual exclusion. Defines a sequence of operations. It is atomic in the presence of multiple clients and failures
* Operation:
    * `BeginTransaction`
        * `read`'s & `write`'s
    * `EndTransaction`
    * `Commit` or `Abort`
* "ACID" Properties:
    * **Atomic**: all or nothing (committed in full, or forgotten if aborted)
    * **Consistent**: transaction does not violate system invariants
    * **Isolated**: transactions don't interfere with each other
    * **Durable**: after a commit, results are permanent (even in presence of failure)

**Transaction Classifications**
* Flat: sequence of operations satisfying above properties
    - [x] simple
    - [ ] failure: all changes undone
* Nested: Hierarchy
    * How to deal with failure? Abort entire, or commit non-aborted parts only?
    * Parent transaction may commit, even if some sub-transactions abort
    * If a parent aborts, all sub-transactions must abort

* Distributed: (flat) transaction on a distributed system

**Transaction Atomicity Implementation**
* Private Workspace:
    * Perform tentative operations on a _shadow copy_.
    * Shadow State: easy to abort, but needs more space
* Writeahead log: fast to commit, more expensive to abort
* Concurrency Control (Isolation):
    * Simultaneous Transactions may interfere - Consistency and isolation require that there is no interference (otherwise you can get lost update or inconsistent retrieval)
* Conflicts and Serialisability
    * Conflict: operations from different transactions that operate on the same data - _read-write_ or _write-write_ conflicts
    * Schedule: Total ordering of operations
        * A legal (serialisable) schedule is one that can be written as a serial equivalent  
    * _Serial Equivalence_
        * Conflict operations performed in the same order on all data

#### Managing Concurrency
**Transaction Manager**

![]({{ site.baseurl }}/assets/images/transactionmanager.png)

**Locking**
_Pessimistic approach: prevent illegal schedules_
* Basic Lock:
    * Locks must be obtained from scheduler before read or write
    * Scheduler grants/releases locks, and ensures only serialisable lock obtain is possible
    * This doesn't guarantee serialisable schedule
* Two-phase Locking
    * Acquire phase, and then release phase within transaction
        * You can only acquire in the acquire stage, and only release int he release stage
* Issues with locking
    * Deadlock (detect in scheduler and decide to abort a transaction, or timeout)
    * Cascade Aborts
        * `Release(Ti, x) -> Lock(Tj, x) -> Abort(Ti)`
        * Tj will have to abort too because it used `x` afer `Ti`!
        * _Dirty read_ problem: seen value from non-committed transaction
        * Solution: strict two-phase locking (release all locks at commit/abort)

**Timestamp Ordering**
_Pessimistic_
* Each transaction has unique timestamp, and each operation receives the transaction timestamp
* Each data item has two timestamps:
    * Read - transaction that most recently read the data item
    * Write - committed transaction that most recently wrote to data item
* Also have tentative timestamps (non-committed writes)
* Timestamp ordering rule:
    * Write request only valid if timestamp of read >= timestamp of last write
    * Read request only valid if timestamp > timestamp of last write

**Optimistic Control**
* Assume no conflicts occur (detect conflicts at commit time)
* Three phases:
    1. Working (using shadow copies)
    2. Validation
    3. Update
* Validation:
    * Need to keep track of read set andw rite set during working phase
    * Make sure conflicting operations with _overlapping_ transactions are serialisable:
        1. Make sure Tv doesn't read items written by other Ti's
        2. Make sure doesn't write any items read by other Ti's
        3. Make sure Tv doesn't write items written by other Ti's
        * (Either other transactions committed already, or other transactions not committed yet)
    * Need to prevent overlapping of validation phases
    * Implementations:
        * Backward validation
            * Check committed overlapping transaction
            * Only have to check if Tv read something that Ti has written (read-write only)
            * On conflict: have to abort Tv
            - [ ] Have to keep track of all old write sets
        * Forward Validation
            * Check non-committed overlapping transactions
            * Only have to check if Tv wrote something another Ti has read
            * On conflict: abort Tv, abort Ti or wait
            - [ ] Read sets of non-committed transactions may change during validation!

#### Distributed Transactions
* A single transaction will involve several servers (may require several services, and store files on several servers)
* All servers must agree to _Commit_ or _Abort_
* Transaction management can be centralised or distributed.

![]({{ site.baseurl }}/assets/images/transaction-df.png)

![]({{ site.baseurl }}/assets/images/transaction-dn.png)

**Distributed Concurrency Control**

![]({{ site.baseurl }}/assets/images/d-concurrency.png)

* Locking
    * Centralised 2PL: single server handles all locks
    * Primary 2PL: Each data item is assigned a primary copy
        * Scheduler on that server is responsible for locks
    * Distributed 2PL:
        * Data can be replicated
        * Scheduler on each machine responsible for locking own data
            * Read lock: contact any replica
            * Write lock: contact all replicas
* Distributed Timestamps
    * Timestamp assigned by first scheduler accessed (clocks have to be roughly synchronised)
* Distributed Optimistic Control
    * Validation operations distributed over servers
    - [ ] Commitment deadlock (because of mutex on validation)
    * Parallel validation protocol
    * Make sure that transaction is seralised correctly

**Atomicity and Distributed Transactions**
* Distributed Transaction Organisation:
    * Each distributed transaction has a _coordinator_ (the server handling the initial `BeginTransaction` procedure)
        * Coordinator maintains a list of _workers_ (other servers involved in the transaction). Each worker must know it's coordinator
        * Coordinator is responsible for ensuring that the whol transaction is atomically committed or aborted (_distributed commit protocol_)
* Distributed Atomic Commit
    * Transaction may only be able to commit when when all workers are ready to commit
    1. Voting Phase: all workers vote on commit (send `CanCommit` msg to all workers)
    2. Completion phase: all workers commit or abort according to decision (Send either `DoCommit` or `DoAbort` to all workers)
    * Issues:
        * Once node has voted _yes_, it can't change
        * If coordinator crashes, all workers may be blocked
            * Could resolve by detecting coordinator crash - talking to other workers to resolve commit
* Two-phase commit of nested transactions
    * required in case worker crashes after provisional commit
    * On `CanCommit` worker: votes _no_ if it has no idea of subtransactions
        * Otherwise: aborts subtransations of aborted transations, saves provisionally-committed transactions and votes _yes_.

### Elections
> When algorithm finished, all processes agree who new coordinator is

* Determining a coordinator:
    * Assume all nodes have unique ID
    * Election: agree on which non-crashed process has largest ID
* Election Requirements:
    * Safety (process either doesn't know coordinator, or knows the process with largest ID), Liveness (eventually, a process crashes or it knows the coordinator)
* Bully Algorithm
    * Message Types:
        * _Election_ (announce election e.g. when we notice coordinator crashed)
        * _Answer_ (if you don't get an answer, you are the coordinator)
        * _Coordinator_ Announce elected coordinator
    * If the highest-numbered process gets an election message, it can immediately respond with coordinator message
* Ring Algorithm:
    * Message Types:
        * _Election_ (forward election data)
        * _Coordinator_ (announce elected process)
    * Every node adds it's own ID to the election message. When the election message arrives at the starting point, it determines one to be the coordinator, and forwards the coordinator message around ring6

### Multicast
* Multicast: single send to a group of receivers
    * Group membership is transparent (sender doesn't need to know who's in the group)
* Uses:
    * Fault Tolerance
    * Service Discovery
    * Performance
    * Event/Notification Propagation
* Properties
    * Group Membershp: Static or Dynamic
    * Open (all users can send) vs. Closed group
    * Reliability
        * Communication failure vs. process failure
        * Guarantee of delivery (All or none, all non-failed)
    * Ordering
        * Guarantee of ordered delivery (FIFO, Causal, Total Order)
* Issues:
    * Performance
        * Bandwidth
        * Delay
    * Efficiency
        * Avoid sending message multiple times (_stress_)
        * Distribution Tree
        * Hardware support (e.g. Ethernet broadcast)
    * Network vs. Application -level
        * Routers understand multicast
        * Applications (middleware) send unicast to group members
        * Overlay distribution tree

**Network-level Multicast**
> "You put packets in at one end, and the network conspires to deliver them to anyone who asks"

<small>~ Dave Clark</small>

* Ethernet Broadcast (send to MAC `FF:FF:FF:FF:FF:FF`)
* IP Multicast
    * Multicast group: class D internet address
    * 244.0.0.0 to 239.255.255.255
    * Multicast routers:
        * Group management: IGMP
        * Distribution Tree: PIM
* Have to use the network abstraction: can't modify the way it operates

**Application-level Multicast**

![]({{ site.baseurl }}/assets/images/applicationmulticast.png)

* Basic Multicast:
    * No reliability or ordering guarantees
    ```c
        B_send(g, m) {
            for each (p in g) {
                send (p, m);
            }
        }

        deliver(m) {
            B_deliver(m);
        }
    ```
* FIFO Multicast
    * Order maintained _per sender_
    ```c
        FO_init() {
            S = 0; // local sequence #
            for (i = 1 to N) V[i] = 0; // vector of last seen seq #s
        }

        FO_send(g, m) {
            S++;
            B_send(g, <m,S>); // multicast to everyone
        }

        B_deliver(<m,S>) {
            if (S == V[sender(m)] + 1) {
                // expecting this msg, so deliver
                FO_deliver(m);
                V[sender(m)] = S;
            } else if (S > V[sender(m)] + 1) {
                // not expecting this msg, so put in queue for later
                enqueue(<m,S>);
            }

            // check if msgs in queue have become deliverable
            for each (<m,S> in queue) {
                if (S == V[sender(m)] + 1) {
                    FO_deliver(m);
                    dequeue(<m,S>);
                    V[sender(m)] = S;
                }
            }
        }
    ```
* Causal Multicast
    * Order maintained between _causally related_ sends

    ```c
    CO-init() {
        // vector of what we’ve delivered already
        for (i = 1 to N) V[i] = 0;
    }

    CO-send(g, m) {
        V[i]++;
        B-send(g, <m,V>);
    }
    B-deliver(<m,Vj>) { // j = sender(m)
        enqueue(<m,Vj>);
        // make sure we’ve delivered everything the message
        // could depend on
        wait until Vj[j] == V[j] + 1 and Vj[k] <= V[k] (k!= j)
        CO-deliver(m);
        dequeue(<m,Vj>);
        V[j]++;
    }
    ```
* Totally-ordered Multicast
    * Every receiver should see messages in the same order
    * Sequencer-based approach
        * Global sequencer sends a message telling all nodes the sequence number of a message broadcast by another node
        * Receivers reorder messages pre processing them
    * Agreement-based approach
        * Send message to everyone, and then receiver nodes replies with a proposed ordering for the message. Sender then decides which is the best and sends that back to all nodes
    * More expensive (increased communications)
    * Other implementation options:
        * Moving sequencer to distribute load
        * Logical clock-based (i.e. lamport clocks) - difficult
        * Token-based
        * Physical clock ordering
* Hybrid Ordering
    * FIFO and Total (since total ordering doesn't guarantee FIFO ordering)
    * Causal and Total
* Dealing with failure in processes and communication
