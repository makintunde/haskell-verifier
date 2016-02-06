import Data.Maybe
import Data.List

type World = [Char] 

type State = [Char]

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
         | IfElse Exp Exp
         | Until Exp Exp 
         | A Exp
         | G Exp
         | F Exp
         | X Exp
         | E Exp
         deriving (Show, Eq)

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
exp3 = IfElse 
         (IfElse 
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

eval k w (IfElse e1 e2)  
  | eval k w e1 = eval k w e2
  | otherwise = True

relatedTo :: World -> Relations -> [World] 
relatedTo w [] = [] 
relatedTo w ((w', w''):ws) 
  | w == w' = w'' : relatedTo w ws
  | otherwise = relatedTo w ws

satEX e s rs 
  = nub [s' | (s', _) <- rs] 
    where 
      x = sat e s

satAF' e s xs ys 
  = undefined
-- | xs == ys = ys
-- | otherwise 
--   = union ys [s' | (s', s'') <- ]

satAF e s rs 
  | x == y = y
  | otherwise = satAF' e s x y
    where
      x = s
      y = sat e s rs

satEU e1 e2 s rs
  = undefined

sat :: Exp -> [State] -> Relations -> [State]
sat (Constant True) s _ = s
sat (Constant False) s _ = []
sat (Variable p) s _ = undefined --TODO
sat (Not e) s rs = (\\) s (sat e s rs)
sat (And e1 e2) s rs = intersect (sat e1 s rs) (sat e2 s rs)
sat (Or e1 e2) s rs = union (sat e1 s rs) (sat e2 s rs)
sat (IfElse e1 e2) s rs = sat (Or (Not e1) e2) s rs
sat (A (X e)) s rs = sat (Not (E (X (Not e)))) s rs
sat (A (Until e1 e2)) s rs
  = sat (Not (Or (E ( Until (Not e2) (And (Not e1) (Not e2)))) (E (G (Not e2))))) s rs
sat (E (F e)) s rs = sat (E (Until (Constant True) e)) s rs
sat (E (G e)) s rs = sat (Not (A (F (Not e)))) s rs
sat (A (G e)) s rs = sat (Not (E (F (Not e)))) s rs
sat (E (X e)) s rs = satEX e s rs
sat (A (F e)) s rs = satAF e s rs
sat (E (Until e1 e2)) s rs = satEU e1 e2 s rs

---------------------------------------------------------------------
runTests 
  = eval kripkeModel w4 exp1 == False &&
    eval kripkeModel w4 exp2 == True  &&
    eval kripkeModel w3 exp1 == False &&
    eval kripkeModel w3 exp3 == False &&
    eval kripkeModel w2 exp4 == False &&
    eval kripkeModel w2 exp5 == False &&
    eval kripkeModel w1 exp6 == True  &&
    eval kripkeModel w2 exp7 == True      
