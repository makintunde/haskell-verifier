# Haskell-Verifier

A model checker for temporal logic formulae. Currently supports Kripke models and CTL formulae.

# Usage
A toy example given a model and a formula:
```
ghci verifier.hs

> let relations = [("w2", "w1"), ("w3", "w2"), ("w4", "w2"), ("w4", "w3"), ("w3", "w3")]
> let worlds = ["w1", "w2", "w3", "w4"]
> let valuations = [("p", ["w2", "w4"]), ("q", ["w2", "w3", "w4"])]
> let kripkeFrame = (worlds, relations)
> let kripkeModel = (kripkeFrame, valuations)
> let exp = Box (Variable "p")
> let world = "w3"

> eval kripkeModel world exp

False

> let ctlExp = E (Until (Variable "p") (Not (A (F (Variable "q")))))
> let states = ["s0","s1","s2","s3"]
> let relations = [("s0","s3"),("s0","s1"),("s1","s1"),("s1","s2"),("s2","s0"),("s2","s3"),("s3","s0")]
> let valuations = [("p",["s0","s2"]),("q",["s0","s3"]),("r",["s3","s1"]),("t",["s2"])]
> let ctlModel = (states, relations, valuations)

> sat ctlExp ctlModel

["s1","s0","s2"]
```

The reason why the first example is "False" is because there exists w3 such that (w4, w3) is in our set of relations, 
but (M, w3) isn't in our set of valuations for p.

The second example produces the set of states which satisfy the given CTL formula in the specified model (consisting of some states, relations and valuations).

# Current Progress
## Done
* Kripke Models
* CTL Formulae

## In Progress
* LTL Formulae

## Requirements
The Glorious Glasgow Haskell Compilation System - version 7.6.3 or later.

## Testing
To run all tests, within `ghci` run:
```
runAllTests
```
