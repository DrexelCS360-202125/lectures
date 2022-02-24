{-# OPTIONS_GHC -Wall #-}

import Prelude hiding (lookup)

--
-- While language
--

type Z = Integer

type Var = String

data Aexp = Const Z | Var Var
          | Add Aexp Aexp | Sub Aexp Aexp | Mul Aexp Aexp
  deriving (Show)

data Bexp = BTrue | BFalse
          | Eq Aexp Aexp | Le Aexp Aexp
          | Not Bexp | And Bexp Bexp
  deriving (Show)

data Stm  = Assign Var Aexp | Skip | Seq Stm Stm
          | If Bexp Stm Stm
          | While Bexp Stm
  deriving (Show)

--
-- Functions for keeping track of program state
--

-- Fix this type definition!
type State = Var -> Z

empty :: State
empty v = error $ "Unbound variable: " ++ v

insert :: Var -> Z -> State -> State
insert v z s = \v' -> if v' == v then z else s v'

lookup :: Var -> State -> Z
lookup v s = s v

--
-- Big-step semantics for arithmetic expression
--

abig :: State -> Aexp -> Z
abig _ (Const z)   = z
abig s (Var v)     = s v
abig s (Add e1 e2) = abig s e1 + abig s e2
abig s (Sub e1 e2) = abig s e1 - abig s e2
abig s (Mul e1 e2) = abig s e1 * abig s e2

--
-- Big-step semantics for Boolean expression
--

bbig :: State -> Bexp -> Bool
bbig _ BTrue       = True
bbig _ BFalse      = False
bbig s (Eq e1 e2)  = abig s e1 == abig s e2
bbig s (Le e1 e2)  = abig s e1 <= abig s e2
bbig s (Not e)     = not (bbig s e)
bbig s (And e1 e2) = bbig s e1 && bbig s e2
