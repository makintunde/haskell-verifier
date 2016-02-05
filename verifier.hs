import Data.Maybe

type World = [Char] 

type Relations = [(World, World)] 

type Valuations = [(String, [World])] 

type KripkeFrame = ([World], Relations) 

type KripkeModel = (KripkeFrame, Valuations) 

data Exp = Constant Bool 
         | Variable String 
         | Not Exp 
         | Box Exp
         | Diamond Exp
         | And Exp Exp
         | Or Exp Exp
         | IfThen Exp Exp
         deriving Show

relations = [("w2", "w1"), 
             ("w3", "w2"), 
             ("w4", "w2"), 
             ("w4", "w3"), 
             ("w3", "w3")]

worlds = ["w1", "w2", "w3", "w4"]

valuations = [("p", ["w2", "w4"]), 
              ("q", ["w2", "w3", "w4"])]

kripkeFrame = (worlds, relations)

kripkeModel = (kripkeFrame, valuations)

exp1 = Box (Variable "p")
exp2 = Diamond (Variable "p")
exp3 = IfThen 
         (IfThen 
           (Diamond (Variable "q"))
           (Diamond (Not (Variable "p")))) 
         (And 
           (Not (Variable "p")) 
           (Not (Variable "q"))) 
exp4 = Box (Constant False)
exp5 = Diamond (Constant False)
exp6 = And (Box (Constant True)) (Box (Constant False))
exp7 = Or (Diamond (Constant True)) (Diamond (Constant False))

w1 = "w1"
w2 = "w2"
w3 = "w3"
w4 = "w4"

---------------------------------------------------------------------

lookUp x xs = fromJust (lookup x xs)

eval :: KripkeModel -> World -> Exp -> Bool

eval _ _ (Constant True) = True

eval _ _ (Constant False) = False

eval (((ws', rs), vs)) w (Box e) 
  = and (map f ws)
    where
      f w' = eval (((ws', rs), vs)) w' e
      ws  = relatedTo w rs

eval k w (Diamond e) = not (eval k w (Box (Not e)))

eval ((_, vs)) w (Variable p) = elem w (lookUp p vs)

eval k w (Not e) = not (eval k w e)

eval k w (And e1 e2) = (eval k w e1) && (eval k w e2)

eval k w (Or e1 e2) = (eval k w e1) || (eval k w e2)

eval k w (IfThen e1 e2)  
  | eval k w e1 = eval k w e2
  | otherwise = True

relatedTo :: World -> Relations -> [World] 
relatedTo w [] = [] 
relatedTo w ((w', w''):ws) 
  | w == w' = w'' : relatedTo w ws
  | otherwise = relatedTo w ws

test 
  = eval kripkeModel w4 exp1 == False &&
    eval kripkeModel w4 exp2 == True  &&
    eval kripkeModel w3 exp1 == False &&
    eval kripkeModel w3 exp3 == False &&
    eval kripkeModel w2 exp4 == False &&
    eval kripkeModel w2 exp5 == False &&
    eval kripkeModel w1 exp6 == True  &&
    eval kripkeModel w2 exp7 == True      
