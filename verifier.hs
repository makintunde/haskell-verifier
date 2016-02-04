import Data.Maybe
import Data.String

data World = W [Char] deriving Eq

data Relation = R (World, World)

data KripkeFrame = F ([World], [Relation])

data Valuation = V [(String, [World])]

data KripkeModel = M (KripkeFrame, Valuation)

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
eval (M (_, V vs)) w (Variable p) = elem w (lookUp p vs)
eval k w (Not e) = not (eval k w e)
eval k w (And e1 e2) = (eval k w e1) && (eval k w e2)
eval k w (Or e1 e2) = (eval k w e1) || (eval k w e2)
eval k w (IfThen e1 e2)  
  | eval k w e1 = eval k w e2
  | otherwise = True

box' :: [Relation] -> World -> World  
-- TODO

      

