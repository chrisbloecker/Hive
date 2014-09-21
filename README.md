Hive
================

How to build:

1. clone the repo
2. init a sandbox:
```
cabal sandbox init
```
3. install dependencies:
```
cabal install --only-dependencies
```
4. configure the project:
```
cabal configure
```
5. install into the sandboy:
```
cabal install
```