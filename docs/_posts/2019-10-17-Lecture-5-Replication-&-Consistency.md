---
layout: post
categories: TeachingWeek03
---

## Replication
> Making copies of services on multiple machines

* Motivations:
  * Reliability (redundancy)
  * Performance
    * Increasing processing capacity, reducing communication
  * Scalability
    * Prevent overloading (size), avoid communication latency (geography)


* Data Replication
  * _Mirror_ data across multiple file servers
  * _Caching_ part of data on multiple file servers
    * Has concept of 'stale' data
    * Is a dynamic subset of the main data store
* Control Replication
  * Replicate the actual service (e.g. web server). Replicated services access centralised database store
  * Issues: all distributed system challenges!

**Replication Challenges**
* Updates to the data
  * Consistency (how to deal with updates)
  * Update propagation (what do updates look like?)
    * Do you send the entire data again, run an operation on existing data etc.?
* Replica Placement
  * How many replicas?
  * Location of replicas (depends on who is accessing the data)
* Redirection & Routing
  * Which replica should clients use?

### Consistency in Replication
* Do replicas have the same data? What differences are allowed?
* Time:
  * How old is the data? (_staleness_)
  * How old is the data allowed to be? (time, versions)
* Operation Order (correct order, allowed order?)

**Ordering**
* Conflicting operations
  * Read/write conflict
  * Write/write conflict
  * Ordering affects consistency
* Partial / Total Ordering
  * Partial: Ordering of operations of a single client
  * Total: interleaving of all conflicting operations

> **Consistency Model** defines which interleaving operations are valid

* Data Coherence: ordering of operations on a single data item
* Data Consistency: ordering of operations for a whole data store
    * Implies data coherence

## Consistency
### Data-centric Consistency Models
> A contract between a distributed data store and clients, in which the **data store** specifies precisely what the results of read/write operations are

* Strong Ordering:
    * All writes must be performed in the order they are invoked
    * Strict, Sequential, Causal, FIFO
    * Expensive communication requirements
* Weak Ordering:
    * Ordering of _groups of writes_ rather than individual writes
    * Weak, Release, Entry

#### Strong-ordering Consistency Models
**Strict Consistency**
* Any read on data item returns a value corresponding to the result of the most recent write on x
    * Assumes an absolute global time (what does _most recent_ mean?)
    * Assumes instant communication
    * Normal on a uniprocessor
    - [ ] Impossible in a distributed system!

**Linearisable Consistency**
* All operations are performed in a single, sequential order
    * Operations ordered according to global timestamp (but this doesn't necessarily mean the time it was actually done)
    * Program order of each client is maintained
    - [ ] Clock synchronisation is difficult

**Sequential Consistency**
* All operations are performed in _some_ sequential order
    * Most common model used
    * More than one correct sequential order is possible.
    * Program order of each client is maintained.
    * Not ordered according to some notion of time
    * Performance limitation: `read time + write time >= minimal packet transfer time`
        * At least one communication needed for a read or write

**Causal Consistency**
* Potentially causally- related writes are executed in the same order everywhere
    * Causally related operations:
        * _read followed by a write (in same client)_
        * _write followed by a read (on any client)_

**FIFO (PRAM) Consistency**
* Only partial orderings of writes are maintained

#### Weak-ordering Consistency Models
**Weak Consistency**
* Synchronisation variable/operation and define critical section with synchronise operations
    * The synchronisation operation can't be completed until all clients have reached the synchronisation point
* Order of synchronisation operations is sequentially consistent
* Can't perform read/write until all previous synchronisation operations have completed
* Order of synchronisations are sequentially consistent

**Release Consistency**
* Explicit separation of synchronisation tasks:
    * `acquire(S)`: Brings local state up to date
    * `release(S)`: Propagate local updates to all replicas
    * An acquire/release pair defines the _critical region_
* Order of synchronisation operations are FIFO consistent
* Release can't be performed until all previous read/writes are completed
* Read/write operations can't be performed until all previous acquires have completed

**Lazy Release & Entry Consistency**
* Lazy Release: Don't send updates on release (prepare update, but not send out)
    * Acquire causes client to get newest state
* Entry Consistency: each shared data item has it's own synchronisation variable
    * Exclusive and non-exclusive access modes

**Eventual Consistency**
* If no updates take place for a long time, all replicas will gradually become consistent
* Must have:
    * Few read/write or write/write conflicts
    * Clients need to accept and time inconsistency (they need to know they will see stale data)

#### CAP Theory
>* Consistency (linearisability)
* Availability (timely response to operations)
* Partition-tolerance (functions if partitioned - fault tolerance on partition such as network)

_Can only choose two_

**CAP Impossibility**
Consequences for wide-area systems...
* Must choose consistency or availability
* Choosing Availability:
    * Give up on consistency? Or eventual consistency?
* Choosing Consistency
    *  No availability, delayed operations
* You have to have P in a distributed system
    * If your system isn't partition tolerance (e.g. network failure), it can't be distributed

### Client-centric Consistency Models
> Provides guarantees about ordering of operations for **a single client**

* Single client accessing different data store replicas (data isn't shared by clients)
* The effect of an operation depends on the client performing it, and the history of the operations the client has performed
* Data items have an owner
* No write-write conflicts

**Monotonic Reads**
* If a client of seen a value _x_ at some time _t_, it will never see an older version of _x_ at a later time
    * Reads are always newer

**Monotonic Writes**
* A write operation on a data item _x_ is completed before any successive write on _x_ by the same client
* E.g.: appending to log file

**Read Your Writes**
* The effect of a write on _x_ will always be seen by a successive read by the same client
* E.g. shopping cart

**Write follows Reads**
* A write operation on _x_ will be performed on a copy of _x_ that is up to date with the value most recently read by the same client

### Consistency Protocols
_Implementation of consistency models_
* Primary-based Protocols:
    * One main store, with backups
    * Remote-write and local-write protocols
* Replicated-Write Protocols:
    * Active replication
    * Quorum-based protocols

**Remote-Write Protocols**
* Single Server:
    * All writes and reads executed at single server
    - [ ] No Replication!
* Primary-Backup
    * All writes executed at single server, all reads local
    * Updates block until executed on all backups
    - [ ] Poor performance
    - [ ] Still centralised (breaks if primary goes down)

**Local-Write Protocol**
* Migration:
    * Data migrated to local server when accessed
    - [x] Good Performance (when not sharing data)
* Migration-Primary:
    * Multiple readers, single writer
    - [x] Performance for concurrent reads
    - [ ] Poor Performance for concurrent writes

**Active Replication**
* Updates (write operation) sent to all replicas
    * Usually sends _update operations_ to the replicas, and replicas apply the operation to the data
* Need totally-ordered multicast for sequential consistency
* If operation needs to return result: you need to work out if all the results returned are consistent

**Quorum-Based Protocols**
* Choose a subset of replicas to send read/write operations to
    * Set up groups to achieve desired consistency
* Data is versioned (need to make sure you always work on the latest version)
* Read (`Nr`) and Write (`Nw`) Quorum is the size of the groups - you need to contact all of them when reading/writing.
    * Read: contact `Nr` replicas and read the one with highest version number
    * Write: contact `Nw` replicas, and update the data on the latest version
* Constraints:
    * `Nr + Nw > N`: should always overlap so that whenever you do a read, you always get one server with the newest write on it
    * `Nw > N/2`: Write quorums will always overlap, so successive writes will always have at least one replica that overlaps

_Note: see slides for examples!_

#### Push vs. Pull
_How do you send the updates to replicas?_
* Pull (or _client-based_):
    * Updates are propagated **on request**
    * Contact all replicas and get the newest state
    - [x] Good when you have more writes than reads
        * You only have to figure out versioning when a read is performed
    - [ ] Polling delay (whenever a read is performed)
* Push (or _server-based_):
    * Push updates to all replicas whenever a write is performed
    - [x] When low staleness is required
    - [x] Good when you have more reads than writes
    - [ ] Have to keep track of all replicas
    * What to propagate?
        * Data (good when reads `>>` writes)
        * Update operation (lower bandwidth cost)
        * Notification/Invalidation (good when writes `>>` reads)
* Leases:
    * Server 'promises' to push updates until the lease expires
    * Lease length depends on:
        * age (last modified time) - if it was recently updated, it's likely to be updated soon in the future
        * renewal frequency
        * state-space overhead
