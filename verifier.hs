import Data.Maybe

data World = W [Char] deriving (Eq, Show)

data Relations = R [(World, World)] deriving Show

data KripkeFrame = F ([World], Relations) deriving Show

data Valuations = V [(String, [World])] deriving Show

data KripkeModel = M (KripkeFrame, Valuations) deriving Show

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

eval (M (F (ws', rs), vs)) w (Box e) 
  = and (map f ws)
    where
      f w' = eval (M (F (ws', rs), vs)) w' e
      ws  = relatedTo w rs

eval (M (_, V vs)) w (Variable p) = elem w (lookUp p vs)

eval k w (Not e) = not (eval k w e)

eval k w (And e1 e2) = (eval k w e1) && (eval k w e2)

eval k w (Or e1 e2) = (eval k w e1) || (eval k w e2)

eval k w (IfThen e1 e2)  
  | eval k w e1 = eval k w e2
  | otherwise = True

relatedTo :: World -> Relations -> [World] 
relatedTo (W w) (R []) = [] 
relatedTo (W w) (R ((W w', W w''):ws)) 
  | w == w' = W w'' : relatedTo (W w) (R ws)
  | otherwise = relatedTo (W w) (R ws)
 
 
