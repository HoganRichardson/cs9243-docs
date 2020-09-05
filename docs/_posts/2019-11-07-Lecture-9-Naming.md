---
layout: post
categories: TeachingWeek06
---

## Basic Concepts of Naming
_How do we find and identify things in distributed systems?_
* Systems manage many entities of different kinds, identified by different kinds of names:
	* Files (path names)
	* Processes (pids)
	* Users (UID, usernames)
	* Hosts (hostnames)
	* Devices
* Difficulty with Naming (in distributed systems):
	* Collisions
	* Name lookup: how can you find all names in the system, how can you find the resource of a particular name 

**Naming Elements**
* A _name_ is a string of characters. It refers to an entity
* _Entity_: resource, process, user etc. Operations are performed on entities at access points
* _Address_:
	* Access point is named by an an address
	* Multiple access points per entity
	* Entity's access points may change
* _Identifier_: a name that uniquely identifies an entity
	* 1:1 relation between entities and identifiers (no duplication, reuse)
	* Allows for easy comparison of references (if they have the same identifier, they are the same object). 
* System-oriented names:
	* Machine readable bit strings 
	* (structured or unstructured)
	- [x] easy to store, manipulate and compare
	- [ ] Not easy to remember (hard for numans to use)
* Human-oriented names:
	* Variable length character strings 
	* (usually structured)
		* e.g. domain name, URL format. 
		* Humans can use structure to work out the type of entity
	- [x] Easy to remember and distiguished between
	- [ ] Hard (or more work) for machines to process
* _Name Space_: container for a set of related names
	* Could be flat, hierarchical, tag-based
* _Aliasing_: Another name for an entity
	* Hard links (two paths to an entity in the graph)
	* Soft links (two leafs that store an absolute path to another node)
* _Merging_:
	* Mounting
	* Combining namespaces (e.g. URLs combine protocol, DNS name, port ID, File path)

## Naming Services
> A Naming Service provides a Name Space

* Name Server implements naming service operations
* Operations:
	* `Lookup`
	* `Add`
	* `Remove`
	* `Modify`
* Clients invoke naming service operations

**Name Resolution**
* Mapping a name onto a node (interested in the data stored at the node)
* Path name resolution: start at beginning node and follow each step
* Iterative Resolution:	
	* Resolves each part of the request individually (e.g. per element of path name)
	- [ ] Caching only at resolver
	- [ ] Lots of communication
* Recursive Resolution:
	* Resolver sends entire path name to the first name server, which gets resolved recursively (e.g. DNS)
	- [x] Effective caching at every name server
	- [x] Reduced communication (if nodes are nearby)
	- [x] Name serers can be protected from external access
	- [ ] Higher performance demand on servers
* Implementation Issues
	* Performance and Scalability:
		* Want to limit load on name servers, limit communication (caching)
		* Partitioning: split name space over multiple servers
		* Replication: copy (parts of) name space on multiple name servers
	* Fault Tolerance: replication
	* Authoritative Name Server: stores an entity's original attributes
* Partitioning: Split name space over multiple servers
	* Structured:
		* Split name space into _zones_ according to graph structure
		* Name resolution can use zone hints to quickly find appropriate server
		- [x] Improved lookup performance (knowledge of structure)
		- [ ] Rigid structure
	* Structure-free:
		* Content placed on servers independent of name space
		- [x] Flexible
		- [ ] Decreased lookup performance, increased load on root
* Replication: copy name space to multiple servers
	* Full Replication: 
		* copy complete name space
		- [x] Fast Performance
		- [ ] Size 
		- [ ] Consistency (change propagation)
		- [ ] Administration (who has rights to make changes where?)
	* Partial Replication: 
		* replicate full name servers
		* replicate zones
		- [x] Improved performance, less consistency overhead
		- [x] Less administrative problems
	* Caching
		* Cache query results
		- [x] no administrative problems
		* Types of Caches:
			* Directory
			* Prefix
			* Full names
		* Implementations:
			* Process-local (kept in addr space of process)
			* Kernel
			* User-process (separate shared service)
		* Cache Updates/Consistency:
			* On-use checking
			* Timeout
			* Invalidation
			* Slow propagation

## Attribute-based Naming
> Directory Services

* Attribute-based names contain distibguishing attributes that inform the type of entity the name refers to.
	* E.g. `C=AU/O=UNSW/OU=CSE/CN=WWWServer/Hardware=Sparc/OS=Solaris/Server=Apache`
	* Distinguished name (DN): set of (distinguished) attributes that forms a canonical name of an entity
	* Can look up entities based on some of the attributes
	* Distinguished name mirrors the structure of the name space. 
	* Possible attribute types and name space is defined by a _schema_.

### Directory Services
* Implements a directory
* Operations
	* `Lookup`
	* `Add`
	* `Remove`
	* `Modify`
	* `Search`: Search for entities that have particular attributes
		* Can use partial knowledge (deosn't have to include distinguished attributes)
* Allows browsing and searching (discovery)
* Partitioning: partition based on name space structure (e.g. hierarchy)
* Replication: 
	* Replicate entire directory, or partition
	* Can have R/W and RO replicas (primary/backup)
	* Challenge: how to deal with search?
		* Catalog and cache replicas
		* There is some subset of attributes that are replicated everywhere
* Lookup:
	* Iterative and recursive
* Search:
	* Iterative or recursive (or multicast - uncommon)
	* If searching for non-distinguished attribute, you can't rely on the structure: need to search each node
	* Searching entire name space is expensive/bad scalability
	* Limit searches by speifying _context_
	* Catalog: stores copy of subset of DIB information on each server (so you can optimise search based on catalog)
	* Multiple attribtues mean multiple possible decompositions for partitioning, but only one decomposition can be implemented

## Distributed Hash Tables
* Address Resolution of Unstructured Names:
	* Unstructured names: "random" bit strings (e.g. random key, hash)
	* No location information
	* How to find corresponding address of entity?
	* Simple solution: Broadcasting
		* Resolver broadcasts query to every node. Only nodes that ahve access point will answer
		* E.g. ARP
	* Better solution: Distribtued Hash Tables...
* Hash table (key-value store) as an overlay network. Operations:
	* `put (key, value)`
	* `value = get(key)`
	* `remove(key)`

**Chord: Distributed Hash Table**
* Ring structure with _nodes_ placed in order around the ring
* _Keys_ assigned to first node with `id > key -> successor(key)`, and placed on the ring
	* (Keys and node IP addresses mapped to an identifier)
* Simple Lookup:
	* Successors function: moves to next key node until it finds the appropriate key space, then looks up the node and returns result
	* Recursive RPCs until node with key is found
	* `O(n)` cost
* Scalable Lookup:
	* Routing table at every node (_"finger"_ table)
	* `i`th entry is `successor (n + 2`<sup>`i-1`</sup>`)`
	* Looks up greatest node id in table < `k` (k is target)
	* Exponentially smaller jumps
	* Adding nodes: will be in middle of space and needs to take over some of the key-value pairs that another node is responsible for
		* `stabalize`: ensure successor pointers up-to-date
		* `fix_fingers`: ensure that finger table updated
	* Node failure:
		* Successor list: `r` successors required to handle `r-1` failures
		* Higher level must handle loss of data relating to failure
	* Lookup cost `O(logn)`
	* Finger table size `O(logn)`
