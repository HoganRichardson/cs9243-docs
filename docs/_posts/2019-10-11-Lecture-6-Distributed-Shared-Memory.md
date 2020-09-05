---
layout: post
date: 2019-10-11 15:30
categories: TeachingWeek02
---

## What is DSM?
* Shared memory across multiple computers
    * Memory is actually stored on one of the computers, but it can be accessed by any of the other machines transparently
* Shared global address space
    * The actual memory is stored locally, and the DSM software manages the mapping/transportation of memory between CPUs.
* Transparent Remote Access
    * CPUs don't know where the data is located (don't know if it is remote or local - don't need to see the memory transfer)
    * **Remote Overhead**: Remote access is expensive
* Why DSM?
    * Shared memory is an easy way to program shared data. DSM emulates a shared-memory interface
    * Can easily port existing programs
    * Sharing data structures with pointers becomes easy (don't have to _marshal_ (package up all references to send to another process) - the other process can just dereference into shared memory)
* Drawbacks of DSM
    * Unforeseen overhead (can't know which pages are remote if truly transparent)
    * May overshare if you don't realise how expensive sharing is

### Ways to implement DSM
* Hardware
    * Early parallel computers (multiprocessor machines)
* OS with Hardware Support
    * SCI network cards map extended physical address space to remote nodes
    * OS maps shared virtual address space onto SCI address range
* OS and Virtual Memory
    * Virtual memory (page faults, paging)
    * Local address space vs. large
* Middleware
    * Library routines to create and access shared memory (transparency is slightly broken)
    * Language-based: encapsulated in language constructs (Shared objects in OO)
* Userspace Implementation
    * Most widely used (and assignment 1)
    * Required from kernel:
        * Need user-level fault handlers (e.g. UNIX signals)
        * User-level VM page mapping and protection (`mmap()` and `mprotect()`)
        * Message passing layer (e.g. socket API)

### DSM Models
* Shared page (coarse-grained)
    * Traditional model
    * _False Sharing_: two different sections of data on same page, but not actually sharing the data
    * What is the ideal page size?
        * Small: reduces false sharing, but will create more communication
        * Large: good for sharing large amount of data, and less frequent communication
* Shared region (fine-grained)
    * Share regions that are smaller than pages, which helps to prevent false sharing
    * Less transparency as it is not regular memory sharing
* Shared Variable
    * Requires more work to maintain consistency (more complex)
* Shared Structure
    * Sharing encapsulated data
    * Tightly-integrated synchronisation
        * and can hide consistency model within the object access functions
    * Loss of familiar shared-memory model
    * Tuple Space:
        * _"Virtual Space"_ where shared data is located
        * To use shared data, perform lookup in the tuple space (which removes it from the space so you can edit it)

### Applications of DSM
* Scientific Parallel Computing
    * Large data processing, simulations
* Graphics
* Data Servers (distributed file system, web server)
* Data Storage

### Goals & Requirements for DSM
1. Transparency
    * Location
    * Migration
    * Replication
    * Concurrency
1. Reliability
    * Computations depend on availability of data: if a node goes down, you loose data!
1. Performance
    * Important in high-performance computing (and for scaling)
    * If you want transparency, it has to be as fast as a memory access would normally be
1. Scalability
    * Important in WAN implementations (probably won't scale well to a WAN)
    * Important in high-performance computing
1. Consistency
    * Access should be consistency (but it is expensive to implement)
1. Programmability
    * Easy to program, don't have to deal with communication
