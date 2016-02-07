# Haskell-Verifier

A model checker for temporal logic formulae.

# Usage
A toy example given a model and a formula:
```
> let relations = [("w2", "w1"), ("w3", "w2"), ("w4", "w2"), ("w4", "w3"), ("w3", "w3")]
> let worlds = ["w1", "w2", "w3", "w4"]
> let valuations = [("p", ["w2", "w4"]), ("q", ["w2", "w3", "w4"])]
> let kripkeFrame = (worlds, relations)
> let kripkeModel = (kripkeFrame, valuations)
> let exp = Box (Variable "p")
> let world = "w3"

> eval kripkeModel world exp

False
```

The reason why the above example is "False" is because there exists w3 such that (w4, w3) is in our set of relations, 
but (M, w3) isn't in our set of valuations for p.

# Current Progress
## Done
* Kripke Models

## In Progress
* *CTL Models*
* LTL Models
