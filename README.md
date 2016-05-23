# Haskell-Verifier

A model checker for temporal logic formulae. Currently supports Kripke models and CTL formulae.

## Usage

### Interactive mode
Verifier can be ran interactively within `ghci`, with example usage as follows:
```
ghci verifier.hs
```

```haskell
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

### Standalone Mode (In Progress)
To compile, run:
``` 
ghc --make verifier
```

To run as a standalone program (in progress), run:
```
./verifier
```

You are then displayed several prompts to input the relevant information
for your model and formula. 

```
Enter an integer to choose the desired mode.
---------------------
  (1) Kripke Models
  (2) CTL
---------------------
2

Enter the set of states in the form 's0 s1 s2 ...' :
s0 s1 s2 s3

What is s0 related to?
s1 s3

What is s1 related to?
s1 s2

What is s2 related to?
s0 s3

What is s3 related to?
s0 

The relations are: 
s0 -> s1
s0 -> s3
s1 -> s1
s1 -> s2
s2 -> s0
s2 -> s3
s3 -> s0

Enter the set of atoms in the form 'p q r ...' :
p q r t

At which states are p true?
s0 s2

At which states are q true?
s0 s3

At which states are r true?
s1 s3

At which states are t true?
s2

pi is defined as: 
pi(p) = { s0 s2 }
pi(q) = { s0 s3 }
pi(r) = { s1 s3 }
pi(t) = { s2 }

Enter the formula to evaluate: 
E ( Until ( Variable "p" ) ( Not ( A ( F ( Variable "q" ) ) ) ) )
```
```
The set of states on which the formula is satisfied are:
s1 s0 s2
```



## Current Progress
### Done
* Kripke Models
* CTL Formulae

### In Progress
* LTL Formulae
* Epistemic Modalities
* A front-end

## Requirements
The Glorious Glasgow Haskell Compilation System - version 7.6.3 or later.

## Testing
To run all tests, within `ghci` run:
```
runAllTests
```
