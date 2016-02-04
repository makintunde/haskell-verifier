# haskell-verifier

A verifier for logical formulas.

# Usage
A toy example given a model and a formula:
```
> let relations = [(W "w2",W "w1"),(W "w3",W "w2"),(W "w4",W "w2"),(W "w4", W "w3"), (W "w3",W "w3")]
> let worlds = [W "w1",W "w2",W "w3",W "w4"]
> let valuations = V [("p", [W "w2", W "w4"]), ("q", [W "w2", W "w3", W "w4"])]
> let kripkeFrame = F (worlds, R relations)
> let kripkeModel = M (kripkeFrame, valuations)
> let exp = Box (Variable "p")
> let world = W "w3"

> eval kripkeModel world exp
False
```

The reason why the above example is "False" is because there exists w3 such that (w4, w3) is in our set of relations and (M, w3) isn't in our set of valuations for p.
