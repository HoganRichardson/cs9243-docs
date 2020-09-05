---
layout: post
date: 2019-11-14 15:00
categories: TeachingWeek07
---

## Security Framework
* **Confidentiality**
* **Integrity**
* **Availability**

* Security Threats (in distributed systems)
	* Interception
	* Interrupction
	* Modification
	* Fabrication

* Attacking Distributed Systems
	* Attacking communication channel:
		* Eavesdrop
		* Masquerade
		* Message Tamporing
		* Denial of Service
	* Attacing Interfaces
		* Unauthorised Access
		* Denial of Service
	* Attacking Systems (Applications, OS, Hardware)
* Security Mechanisms for Distributed Systems
	* Authentication
	* Authorisation
	* Auditing (trace which entities access which resources)
	* Confidentiality
	* Integrity
* Challenges of Security
	* Weakest Link
	* Complexity
	* Pervasiveness (attacks anywhere in the application stack)
* Mechanisms for making security easier
	* Distribution of Mechanisms:
		* Trusted Computing Base: parts of the system taht are able to compromise security (the samller the better)
		* May have to implement key services yourself
		* Physically-separate security services from other services
	* Simplicity

## Security Foundations
### Cryptography
**Ciphers**
* Idea: infeasible to reconstruct the message from the ciphertext without the key
* Encryption: want a function that's easy to compute, but hard to reverse without decription key (_"one-way functions"_)
* Attacks on Ciphers:
	* Cyphertext only attacks
	* Known plaintext attacks
	* Chosen plaintext attacks
	* Brute-force attacks
* Confusion: every bit of the _key_ influences large number of ciphertext bits 
* Diffusion: every bit of the _plaintext_ should influence a large number of ciphertext bits
* Should not be dependent on user selecting 'good' keys
* Usually based on problems that are provably hard to invert
* Key Length:
	* Longer keys make it harder to brute-force
	* But it also slows down the encryption and decryption operations

_Examples_
* Substitution Ciphers
	* Each plaintext character replaced with a ciphertext character
	* Caesar cipher, book cipher (replace words by location of word in book)
	* Easy to break (vulnerable to letter frequency attack etc.)
* One-time Pad
	* Random string XOR'ed with the plaintext
	* Theoretically secure
	* Require random string to have no pattern, not reused, and be known by both parties (key distribtion problem). 
* Symmetric Ciphers
	* Shared key (the key is the secret)
	* Fast
	* Challenge: need a secure channel to establsh the shared key
	* Need `n*n` keys for `n` users (key for each pair!)
	* Examples: Tiny Encryption Algorithm (TEA), DES, AES
* Asymmetric Ciphers
	* Public/Private key pair (encrypted with public key, decrypted with private key)
	* Only need 1 public key per user (so only `n` keys for `n` users)
	* Slower for encrypting large volumes of data
	* Examples: RSA, Diffie Hellman
* Block Ciphers
	* Encrypt fixed-sized blocks of data one at a time
	* Usually requires padding in last block (vulnerable to plaintext attack, spot patterns)
	* If each block is independent, vulnerable to pattern analysis
	* Cipher Block Chaining:
		* Feed cipher of last block into the next block
	* Good if you have large chunks of data
* Stream Cipher
	* Encode plaintext bit by bit
	* Useful for streams of data (e.g. networks)
	* XOR keystream with datastream
	* Shared seed to PRNG that is used to generate keystream
	* NOT a one time pad

**Signatures & Digests**
* Used to ensure message integrity
* Secure digest/hash 
* Function that takes message and produces a fixed-length value. 
* Should be difficult to find collisions
* Any symmetric encryption algorithm _could_ be used as a hashing function (but less efficient, requies key). Hashes usually only require plaintext.
* Must be resillient to:
	* Collision Attack (find any two messages that hash to same thing)
	* Preimage attack (given hash, find a message that hashes to message)
	* Second preimage attack (given a message, find another message that hashes to same value)
* Digital Signature:
	* Used to verify who sent the message: integrity, authenticity and non-repudiation
	* Send message and encrypted hash of message

### Security Protocols
* Uses encryption, signatures etc.
* Mechanisms:
	* Challenge-Response: use nonce 
	* Ticket: secured information to be paseed to another party
	* Session keys: for secure communication
* Principles: 
	* message must have all relevant information (don't rely on a receiver state)
	* Don't allow parties to do things identically
	* Don't give away valuable information to strangers
* Examples of attacks:
	* Man in the middle (and relay)
	* Replay
	* Message Manipulation

**Key Distribution**
* Needham-Schroeder Protocol
	* Central key distribution centre
	* Each agent shares a symmetric key with the distribution centre
	* Central centre generates new keys for users to communicate

**Secure Communication**
* SSL(/TLS)
	* Uses asymmetric keys
	* Server sends certificate with public key
* Secure Group Communication
	* Confidential:
		* All group members share same key OR
		* Separate keys for each pair OR
		* Public Key Cryptography 
		* (issues with key distribution, trust etc.)
	* Secure Replicated Servers:
		* Protect from malicious group memebrs
		* Collect responses from all servers to authenticate 
		* Not transparent
		* Secret Sharing: all group members know part of a secret

### Authentication
* Requirements:
	* Representation of Identity (UID, etc.)
	* Way to verify Identity (password etc.)
* Credentials:
	* _Speak_ for a principal (e.g. certificate)
	* Combinations of credentials, role-based cretendials
* Approaches to Authentication:
	* Passwords
	* Shared Secret Key	(challenge-response)
	* Key Distribution Centre
	* Public Key
	* Hybrid approach
* Kerberos
	* ...[TODO!!!]
	* Centralised shared key (based on symmetric encryption)
	* Implementation of Needham-Schroeder 
* Weaknesses of Implementation of Needham-Schroeder 
	* Key distribution is centralised
	* Compromised keys can be used to decrypt past communications
* Certificates
	* Can use authentication to guarantee sender authenticity, but have no way of knowing that you actually provided the right key to start with (certificates!)
	* Certs are a way of linking (identntiy, public key)
	* Distriuted certificate servers/directories that provides certs. (certs issueed by certificate authorities)
	* Checking Certificate trust:
		* Trust in the certificate authority
		* Recursive certification until you reach root authority (trust the root authorities)
	* Need means of revoking trust to certificate (usually have expiry date)

### Authorisation & Access Control
> Determining what actions an authenticated entntiy is authorised to perform

* Access Rights: rights required to access (or perform operation on) a resource
	* Acess Control: Verify access rights
	* Authorisation: Grant access rights
* In non-distributed systems:
	* Global mechanisms and policies (e.g. users, file permissions, separate address spaces)
* Distributed Systems:
	* Server specific (web servers and `.htaccess`)
	* Application specific

#### Access Control
**Access Control Matrix**
 
Subjects | Object 1 | Object 2 | Object 3 | Object 4 
---|---|---|---
S1 | `terminate` | `wait`, `signal`, `send` | `read` | - 
S2 | `wait`, `signal`, `terminate` | - | - | `read`, `execute`, `write` 
... | 

* Usually a dymanic data structure
* Can have permanent (e.g. `chmod`) or temporary (e.g. `suid`) changes
* Often quite sparse, and with many repeated entries (usulaly not stored explicitly)

**Design Considerations in a protection system**
* Propagation of Rights
	* Can someone act as an agent's proxy?
* Restriction of Rights
	* Can agent propagate subset of their rights?
* Amplification of Rights
	* Can an unprivaleged agent perform some privileged operation?
* Revocation of Rights
	* Can a right be removed?
* Determine object accessibility:
	* Can we easily identify which subjects can access an object
* Determine an agent's protection domain
	* What set of objects can a subject acces?

**Implementation: ACL**
* Column-wise representation of access matrix
* Properties of ACL's
	* Propagation: need to have meta-right to change ACL (e.g. owners have right to perform `chmod`)
	* Restriction: need to hav emeta-right to change ACL
	* Amplification: Yes (suid flags)
	* Revocation: remov from ACL
	* Object accessibility: explicit in ACL
	* Protection domain: hard/impossible

**Implementation: Capabilities**
* A capability is an element of the access matrix
* C-list is a row-based representation of access matrix (i.e. capabilities associated with each subject)
* Capabilities can usually be used as an object name
* Properties of Capabilities:
	* Propagation: copy capability (since the capability is a reference) - need to be cautious of confinement
	* Restriction: may be supported by _derived_ capability (make restricted copy)
	* Amplification: may have amplification capability objects
	* Revocation: Difficult (requires invalidation of capabilities) (think: physical keys to house)
	* Object accessibility: hard/impossible
	* Protection domain: explicit in C-list
* Making capabilities _tamper-proof_:
	* Tagged Capabilities
		* Protected by hardware 
		* Control by OS (only kernel can turn on tag bit)
	* Partitioned (segregated) Capabilities
		* Protected by OS (capabilities kept in kernel space)
	* Sparse capablities
		* Commonly used in distributed systems
		* Protected by sparseness (obscurity)
		* E.g.: Signature Capabilities
			* Tamper proof via encryption with secret kernel key (appends signature that can check integrity)
			* Can be freely passed around
			* Need to encrypt on each validation
		* E.g.: Password Capabilities
			* 'Random' bitstring is password
			* Validation requires checking against global object table 
			* Relies on storing state (problem on replicating global object table)

**Firewalls**
* Disconnects parts of the system from the outside world (incoming communication is inspected/filtered)
* Packet-filtering gateway or Application-level gateway
* Firewalls Misconseptions:
	* MnM security problems (insider attack, once you're in you're in)
	* Content filtering doesn't mean that unsafe content can't pass through (e.g blocking exe doesn't block all malware). 

## Breaking Security
* Encryption:
	* Weakness in algorithms/implementations
	* Underlying intractable problem
	* Brute Force (is best force)
* Protocols
	* Weakness in protocol deasign (MitM, reflection etc.)
	* Vulnerability in implementation
* Authentication
	* Find keys or passwords
	* Social Engineering
* Authorisation
	* Find problems in Access Control matrix
	* Find/exploit bugs to privesc
