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

 
