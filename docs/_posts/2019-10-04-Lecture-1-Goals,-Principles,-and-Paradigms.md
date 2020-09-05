---
layout: post
date: 2019-10-04 12:00
categories: TeachingWeek01
---

## Goals of Distributed Systems
1. Transparency
2. Dependability
3. Scalability
4. Performance
5. Flexibility

### Transparency
_Concealment of the separation of components of a distributed system - single image view. User can't see that the system is distributed._

There are a number of forms of transparency, including the following. Ordered by ease to achieve:
* **Access**: Local/remote resources accessed in the same way (e.g. file system - same commands to access any file)
* **Location**: Users are unaware of the location of resources (e.g. remote files don't have different names, such as `host:filename`)
* **Migration**: Resources can migrate without a name change (You don't have this with Google Drive/Dropbox cloud sync: you have location/access transparency, but file name changes if the file moves to a different location)
* **Replication**: Users unaware of the existence of multiple copies (E.g. you might not see an edit another person has made, so you don't have replication transparency)
* **Failure**: Users are unaware of the failure of individual components
* **Concurrency**: Users are unaware of sharing resources with others

Is transparency always desirable?
* In some situations, it is useful to know that a file is stored remotely (e.g. optimising latency by not reading/writing all the time)
* There are other cases too, where some transparency is useful, and others may not be

Is it always possible?
* Concurrency and failure transparency can be very difficult

### Dependability
* In theory, distributed systems promise higher availability (replication), but availability may degrade (more components, so more points of failure)
    * Constantly have to deal with the threat of failure
* Usually requires all the other goals/principles of distributed systems
    * Dependability requires _consistency, security and fault tolerance_

### Scalability
_A system is scalable if it can handle the addition of users and resources without suffering a noticeable loss of performance or increase in admin complexity_

* Scaling has three dimensions:
    * Size: number of users and resources (may overload the system)
    * Geography: the distance between users and resources (communication problem)
    * Administration: number of organisations that have admin control over parts of the system (administrative mess)
* Scalability often conflicts with (small system) performance: need to ask yourself how big the system is actually going to be - don't need to overengineer your solution.
* 'Scalability' claim is often abused: the meaning isn't clear. Size is undefined/may not relate to desire
* **Vertical Scaling**: "Scaling UP": Increasing resources of a single machine. Not really a distributed system - just replacing one machine with a faster one
* **Horizontal Scaling**: "Scaling OUT": Adding more machines

Techniques for Scaling:
* Hiding communication latencies
* Distribution (spreading data and control around)
* Replication (making copies of data and processes)
* Decentralisation
    * Services (Don't want to have a service running on only one machine)
    * Data (don't centralise directories/storage)
    * Algorithms
        * Don't require any machine to hold the complete system state
        * Allow nodes to make decisions based on local information
        * Algorithms must survive failure of nodes
        * No assumption of a global clock
    * It's hard to write decentralised algorithms

### Performance
* Any system should thrive for max performance, but in distributed systems, performance conflicts with other desired properties (transparency, security, dependability, scalability)

#### Numbers Every Programmer should Know
```
L1 cache reference ...................... 0.5 ns
Branch mispredict ......................... 5 ns
L2 cache reference ........................ 7 ns
Mutex lock/unlock ........................ 25 ns
Main memory reference ................... 100 ns
Compress 1K bytes with Zippy .......... 3,000 ns = 3 us
Send 2K bytes over 1 Gbps network .... 20,000 ns = 20 us
Read 1 MB sequentially from memory .. 250,000 ns = 250 us
Round trip within same datacenter ... 500,000 ns = 0.5 ms
Disk seek ........................ 10,000,000 ns = 10 ms
Read 1 MB sequentially from disk . 20,000,000 ns = 20 ms
Send packet CA->Netherlands->CA . 150,000,000 ns = 150 ms
```
<small>from Peter Norvig, Jeff Dean, see also: [http://www.eecs.berkeley.edu/~rcs/research/interactive_latency.html](http://www.eecs.berkeley.edu/~rcs/research/interactive_latency.html)</small>

### Flexibility
* Build a system out of required components
* Extensibility: Components/serviced can be changed or added
* Openness (and clarity) of interfaces/spec
    * Allows reimplementation and extension
* Interoperability - allow others to add/build onto your system
* Separation of policy and mechanism (standardised internal interfaces)

### Common Mistakes, and Rules of Thumb
**Common Mistakes**
* Reliable network
* Zero latency
* Infinite bandwidth
* Secure network
* Topology does not change
* One administrator
* Zero transport cost
* Everything is homogeneous: people have different versions, different libraries etc.

**'Rules of Thumb'**
* Tradeoffs: many challenges provide conflicting requirements, so need to have trade offs, and understand what is more important
* Separation of Concerns: Split the problem into individual concerns, and address these independently
* End-to-End Argument: Some communication functions can only be reliably implemented at the application level
* Policy vs. Mechanism: System should build mechanisms that allow flexible application of policies (avoid built in policies: e.g. crypto choice)
* K.I.S.S.: Keep it as simple as possible

## Principles and Paradigms
### Key Principles of Distributed Systems
**Principles**
* System Architecture
* Communication
* Partitioning, Replication and Consistency
* Synchronisation and Coordination
* Naming
* Fault Tolerance
* Security

**Paradigms**
* Shared Memory
* Distributed Objects
* Distributed file system (everything is a file)
* Distributed coordination
* Server Oriented Architecture, web services
* Distributed database
* Shared documents (e.g. web: everything's a document)
* Agents
