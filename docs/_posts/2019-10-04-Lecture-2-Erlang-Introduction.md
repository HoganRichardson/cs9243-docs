---
layout: post
date: 2019-10-04 13:00
categories: TeachingWeek01
---
## What is Erlang?

### Variables, Binding and Pattern Matching
* Variables (have capital letters) can be assigned (or 'bound') ONCE!
* 'Anonymous' variables start with `_`

```erlang
[A, B|C] = [1,2,3,4,5,6,7]
% This means A=1,
%            B=2,
%            C=3,4,5,6,7 (list)

[A|B] = [abc]
% This means A='abc'
%        and B is an empty list

{A,_, B} = {123, 456, 789}
% Essentially 'ignores' the 456 item.
%    this way you only bind the elements you wanted to use
```

### Functions
* Functions can have multiple definitions
  * When called, it looks through all the possible function definitions and chooses the first one that matches the arguments
* Can also have anonymous functions
  * Functions that are assigned to variables, and then you can invoke this function, and pass it on to other functions

### Punctuation and Syntax

| Symbol | Purpose |
| --- | --- |
| `,` | AND |
| `;` | OR  |
| `.` | END |

### Concurrent Programming
```erlang
spawn(Mod, Func, Args)

% or alternatively (and more useful)
F = fun() -> io:format("Hello") end.
Pid = spawn(F)
```
* The spawn function creates a new process that evaluates the given function with the provided arguments
  * Typically, you want your spawned process to stay alive
* They can also be invoked using a function variable (second example above)

**Message Passing**

If `B` is a process ID of a spawned process, you can send it a message as follows:
```erlang
% Message Sending
B ! {self(), hello, you}

% Message Receiving
receive
    {From, Msg1, Msg2} -> ...
end

% Enforce ordering of messages (or 'flush')
B!foo
B!bar
% foo always runs before bar
```

Message processing:
* Messages queued
* Test each message against all receive clauses until a match is found (if not found, it goes back into the queue)
* Block and wait for next message
