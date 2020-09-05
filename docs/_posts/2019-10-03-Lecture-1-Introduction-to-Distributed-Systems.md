---
layout: post
categories: TeachingWeek01
---
## Distributed Systems: What and Why

### What is a distributed system?
The Ideal:
> A distributed system is a collection of independent computers that appear as a single coherent system

<small>~ Andrew Tannenbaum</small>

* However, realistically, it's hard to create a system like this. There are several challenges in building 'true' distributed systems.

* Currently, we have what appears to be a coherent system, however when something breaks, it becomes obvious that it is not one system

A revised definition:
> A collection of independent computers that are jointly used to perform a single task or provide a single service

* Collection: more than 1
* Independent Computers: independent systems that can run on their own without the other systems if necessary
* Used jointly: working together
* Single task/service: shared goal

**Examples of Distributed Systems/Applications**
* DNS
* Web server (HTTP)
* Web Applications
* Supercomputer
* Google Search
* ATM Networks, and wider Banking network
* Car, Aeroplane
* Industrial Control Systems (elec, water)
* Active Directory
* Distributed Database (e.g. CockroachDB, Cassandra)
* IoT
* IPFS (distributed Filesystem/blockchain)
* Ceph (distributed filesystem)
* WiFi Network
* Torrent network
* Multiprocessors??
* Tor network
* ROS (Robot OS)
* CDN
* Cloud Computing
* Crowd-sourced computing
* GPS
* CSE Login Servers
* Federated Machine Learning
* OAuth
* Masterdon (Peer social media network)
* IRC

**Advantages of Distributed Systems**
* Cost
  * Buying multiple smaller hardware components is cheaper than buying a system that is large
* Performance
  * Can out-perform the available performance of a single system
* Scalability
  * Grow system as required (e.g. add more storage)
* Reliability
  * Redundancy
* (Inherent) Distribution
  * Some services are inherently distributed (e.g. web)

**Disadvantages of Distributed Systems**
* Adding new component
  * There is now a network component that is relied upon (introduces performance limits)
* Software Complexity
  * Distributed software is more complex, and harder to develop
* Failure
  * More elements that can fail (failure must be dealt with in some way)
* Security
  * Easier to compromise (because of increased complexity, addition of network component etc)

> I can never do any work on a distributed system because a computer I've never heard of has crashed

<small>~ Lamport</small>

## Hardware and Software of Distributed Systems
**Hardware Architecture**

* A device with direct memory access is not a distributed system (e.g. uniprocessor, multiprocessor)
* Multicomputers are distributed systems. They do not have direct access to each other's memory.
  * The computers are connected by some sort of network. In order to access memory on another computer, it must be done via a request on this network
* Homogeneous (all nodes are the same computer/architecture/specs) or Heterogeneous (different resources/capabilities across different nodes)

**Software Architecture**
* Uniprocessor OS:
    * ![]({{ site.baseurl }}/assets/images/uniprocessor.png)
* Multiprocessor OS: Kernel designed to run on multiple CPUs. Uses same communicate primitive as uniprocessor OS. Can share memory between processes etc.
    * ![]({{ site.baseurl }}/assets/images/multiprocessor.png)
  * Generally provides a single system image - no matter which processor is running, the system looks the same
  * Limitations: have to have the same kind of processor
* Network OS: several uni/multiprocessor operating systems running on a multicomputer
    * ![]({{ site.baseurl }}/assets/images/network-os.png)
  * Each machine runs a kernel and their own network and OS services, but have shared applications
  * Distribution of tasks is explicit to user
  * No single image: individual nodes are highly autonomous
  * Application must deal with all distributed system problems: differences across systems, communication, failures etc.
    * All distributed system problems must be solved by the application
* Distributed OS: Shared/distributed operating system services and applications, with independent kernels
    * ![]({{ site.baseurl }}/assets/images/distributed-os.png)
  * High degree of transparency (can see more information about other nodes) - single system image
  * Nodes work together to provide a single memory, network service etc.
  * Usually relies on homogenous hardware
  * Abstracts details away that make it harder to optimise the system
  * More work required to communicate etc. to provide the abstraction
  * Inflexible: have to write applications specifically for the distributed OS
* Middleware Model: Network/OS Services and kernel on individual machines, but provide a shared middleware services layer between OS services and the distributed application
    * ![]({{ site.baseurl }}/assets/images/middleware.png)
  * Has some sort of distributed system interface for solving distributed system processes, but can run on multiple different OS's.
  * Why is this model successful?
    * Builds on commonly-available network OS abstractions
    * Usually runs in userspace (easier to run the system anywhere)
    * Raises level of abstraction for programming
    * Independent from OS/network protocol/programming language etc. making it more flexible
  * Problem: bloated interface/too many features

**Distributed Systems and Parallel Computing**
* Parallel computing is usually focused on improving performance of an application by running it in parallel
  * Shared-memory systems: multiprocessors with direct memory access
  * Distributed memory systems: Multicomputer systems
