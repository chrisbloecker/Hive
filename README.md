Hive
================

How to build:

- clone the repo
- init a sandbox:
```
cabal sandbox init
```
- install dependencies:
```
cabal install --only-dependencies
```
- configure the project:
```
cabal configure
```
- install into the sandboy:
```
cabal install
```