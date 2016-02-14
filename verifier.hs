import Data.Maybe
import Data.List

type World = [Char] 

type State = [Char]

type Relations = [(World, World)] 

type Valuations = [(String, [World])] 

type KripkeFrame = ([World], Relations) 

type KripkeModel = (KripkeFrame, Valuations) 

type CtlModel = ([State], Relations, Valuations)

-- TODO: Weak Until.
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

ctlModel = (["s0","s1","s2","s3"],
            [("s0","s3"),("s0","s1"),("s1","s1"),("s1","s2"),("s2","s0"),("s2","s3"),("s3","s0")],
            [("p",["s0","s2"]),("q",["s0","s3"]),("r",["s3","s1"]),("t",["s2"])])

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
ctlExp1 = A (F (Variable "q"))
ctlExp2 = A (G (E (F (Or (Variable "p") (Variable "q")))))
ctlExp3 = E (X (E (X (Variable "r"))))
ctlExp4 = A (G (A (F (Variable "q"))))
ctlExp5 = E (Until (Variable "p") (Not (A (F (Variable "q")))))


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

-- Determines the set of states satisfying EX e.
satEX e m
  = nub [s' | (s', _) <- rs] 
    where 
      x = sat e m
      (s, rs, vs) = m

-- Builds a map from a state to all states it is related to.
buildRelationMap :: [(State, State)] -> [(State, [State])] -> [(State, [State])]
buildRelationMap [] m
  = m
buildRelationMap ((k,v):rs) [] 
  = buildRelationMap rs [(k,[v])] 
buildRelationMap ((k,v):rs) ((k',vs):rs')
  | k == k' = buildRelationMap rs ((k,(v:vs)):rs')
  | otherwise = buildRelationMap rs ((k,[v]):(k',vs):rs') 

getStatesAll' :: [(State, [State])] -> [State] -> [State]
getStatesAll' [] _ = []
getStatesAll' ((k,vs):rest) y
  | and (map f vs) = k : getStatesAll' rest y
  | otherwise = getStatesAll' rest y
    where 
      f v = elem v y

getStatesAll :: Relations -> [State] -> [State]
getStatesAll rs y
  = getStatesAll' values y
    where 
      values = buildRelationMap rs []

satAF' :: [State] -> [State] -> Relations -> [State]
satAF' x y rs
  | x == y = y
  | otherwise = satAF' x' y' rs
    where 
      x' = y
      y' = union y (getStatesAll rs y) 
-- | xs == ys = ys
-- | otherwise 
--   = union ys [s' | (s', s'') <- ]

-- Determines the set of states satisfying AF e.
satAF :: Exp -> CtlModel -> [State]
satAF e m 
  = satAF' x y rs 
    where
      x = s
      y = sat e m 
      (s, rs, _) = m

getStates :: Relations -> [State] -> [State]
getStates [] _ = []
getStates ((s,s'):ss) y
  | elem s' y = s : (getStates ss y)
  | otherwise = getStates ss y


satEU' :: [State] -> [State] -> [State] -> CtlModel -> [State]
satEU' x y w m
  | x == y = y
  | otherwise = satEU' x' y' w m
    where
      x' = y
      y' = union y (intersect w (nub (getStates rs y)))
      (_, rs, _) = m

-- Determines the states satisfying E[e1 U e2].
satEU :: Exp -> Exp -> CtlModel -> [State]
satEU e1 e2 m 
  = satEU' x y w m
    where
      w = sat e1 m
      x = s
      y = sat e2 m
      (s, _, _) = m

sat :: Exp -> CtlModel -> [State]
sat (Constant True) (s, _, _) = s
sat (Constant False) _ = []
sat (Variable p) (s, _, vs) = [s' | s' <- lookUp p vs]
sat (Not e) (s, rs, vs) = (\\) s (sat e (s, rs, vs))
sat (And e1 e2) m  = intersect (sat e1 m) (sat e2 m)
sat (Or e1 e2) m = union (sat e1 m) (sat e2 m)
sat (IfElse e1 e2) m = sat (Or (Not e1) e2) m
sat (A (X e)) m = sat (Not (E (X (Not e)))) m
sat (A (Until e1 e2)) m
  = sat (Not (Or (E ( Until (Not e2) (And (Not e1) (Not e2)))) (E (G (Not e2))))) m
sat (E (F e)) m = sat (E (Until (Constant True) e)) m
sat (E (G e)) m = sat (Not (A (F (Not e)))) m
sat (A (G e)) m = sat (Not (E (F (Not e)))) m
sat (E (X e)) m = satEX e m
sat (A (F e)) m = satAF e m
sat (E (Until e1 e2)) m = satEU e1 e2 m

---------------------------------------------------------------------
runKripkeTests 
  = eval kripkeModel w4 exp1 == False &&
    eval kripkeModel w4 exp2 == True  &&
    eval kripkeModel w3 exp1 == False &&
    eval kripkeModel w3 exp3 == False &&
    eval kripkeModel w2 exp4 == False &&
    eval kripkeModel w2 exp5 == False &&
    eval kripkeModel w1 exp6 == True  &&
    eval kripkeModel w2 exp7 == True     

runSatTests
  = sort (sat ctlExp1 ctlModel) == ["s0","s2","s3"] &&
    sort (sat ctlExp2 ctlModel) == ["s0","s1","s2","s3"] &&
    sort (sat ctlExp3 ctlModel) == ["s0","s1","s2","s3"] &&
    sort (sat ctlExp4 ctlModel) == [] &&
    sort (sat ctlExp5 ctlModel) == ["s0", "s1", "s2"]
    
runAllTests
  = runKripkeTests && runSatTests
