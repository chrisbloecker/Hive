Hive
================

Hive is a domain specific language, intended to make distributed and parallel programming easier. It provides tools based on a process algebra to create and compose processes and process hierarchies from pure functions as well as an interpreter to run these processes with a given input. Hive is implemented in Haskell, building on top of [Cloud Haskell](http://www.haskell.org/haskellwiki/Cloud_Haskell).

## Idea

When developing parallel algorithms, there is usually a lot of boilerplate code for process communication involved. This code contains actions such as creating communication channels, sending messages, waiting for replies, etc. and is mixed up with code specific to a certain algorithm. The concepts, however are always the same: running processes in parallel, in sequence, making choices between processes etc. Hive provides constructors and combinators to both eliminate this boilerplate code and lift the abstraction for parallel programming to a higher level. Hive leverages Haskell's strong type system to make the construction of processes with mismatching types impossible.

## Example

This is a simplified example, however it shows the idea behind Hive:

Let **f1**, **f2**, **f3** and **g** be functions. Let **Simple** be a process constructor that takes a function and wraps it into a process. Let **Sequence** and **Parallel** be process combinators that combine two processes into one. Let **runProcess** be an interpreter function that runs processes on an input. Let **x** be an input.

Assuming you want to wrap the given functions into processes, you would write something like

```
f1p = Simple f1
f2p = Simple f2
f3p = Simple f3
 gp = Simple g
```

Assuming you want to build a process that will first run **f1p**, then **f2p** and **f3p** in parallel and then use **gp** to combine the output from **f2p** and **f3p**, you would write something like

```
proc = Sequence f1p (Parallel f2p f3p gp)
```

Assuming you want to run **proc** on **x**, you would write something like

```
runProcess proc x
```

The interpreter behind **runProcess** will then take care of parallel execution of **f2p** and **f3p**. Note that it's not possible to run e.g. **f1p** and **f2p** in parallel because they have been modeled to run sequentially.

## How to build Hive:

- clone the repo
- init a sandbox:
```
cabal sandbox --sandbox=/path/to/your/sandbox init
```
- install dependencies:
```
cabal install --only-dependencies
```
- configure the project:
```
cabal configure
```
- install into the sandbox:
```
cabal install
```