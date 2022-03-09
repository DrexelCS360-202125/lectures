module Examples where

import Prelude hiding (Maybe(..), Either(..), mapM)

import System.Random

-- map :: (a -> b) -> [a] -> [b]

--
-- Functors
--
data Maybe a = Nothing
             | Just a
  deriving (Eq, Ord, Show, Read)

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
  deriving (Eq, Ord, Show, Read)

{-
instance Functor [] where
    fmap = undefined
-}

instance Functor Maybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing  = Nothing
    fmap f (Just x) = Just (f x)

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Leaf x)   = Leaf (f x)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

data Either a b = Left a
                | Right b
  deriving (Eq, Ord, Show, Read)

instance Functor (Either a) where
    fmap _ (Left x)  = Left x
    fmap f (Right x) = Right (f x)

--
-- Applicatives
--

instance Applicative Maybe where
    -- pure :: a -> Maybe a
    pure x = Just x

    -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing <*> _ = Nothing
    Just f  <*> x = fmap f x
{-
    Just _  <*> Nothing = Nothing
    Just f  <*> Just x  = Just (f x)
-}

{-
instance Applicative [] where
    -- pure :: a -> [x]
    pure x = [x]

    -- (<*>) :: [a -> b] -> [a] -> [b]
    gs <*> xs = [g x | g <- gs, x <- xs]
-}

--
-- Abstracting over computation
--

instance Monad Maybe where
  -- return :: a -> Maybe a
  return x = pure x

  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= _ = Nothing
  Just x  >>= f = f x

--
-- Interpreter
--
data Exp = Val Int | Div Exp Exp

eval :: Exp -> Int
eval (Val n)   = n
eval (Div x y) = eval x `div` eval y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval' :: Exp -> Maybe Int
eval' (Val n)   = Just n
eval' (Div x y) = case eval' x of
                    Nothing -> Nothing
                    Just n  -> case eval' y of
                                 Nothing -> Nothing
                                 Just m  -> safediv n m

eval'' :: Exp -> Maybe Int
eval'' (Val n)   = Just n
eval'' (Div x y) = eval'' x >>= \n ->
                   eval'' y >>= \m ->
                   safediv n m

--
-- Functions that use random numbers
--

newtype Randomized a = R { runR :: StdGen -> (a, StdGen) }

instance Functor Randomized where
    -- fmap :: (a -> b) -> R a -> R b
    fmap f g = R $ \s ->
        let (x, s') = runR g s
        in
          (f x, s')

instance Applicative Randomized where
    pure x = R $ \s -> (x, s)

    mf <*> mx = R $ \s -> let (f, s')  = runR mf s
                              (x, s'') = runR mx s'
                          in
                            (f x, s'')

instance Monad Randomized where
    mx >>= f = R $ \s -> let (y, s') = runR mx s
                         in
                           runR (f y) s'

--
-- State Monad
--

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap _f _m = undefined

instance Applicative (State s) where
    pure _x = undefined

    _mf <*> _mx = undefined

instance Monad (State s) where
    return = undefined

    _mx >>= _f = undefined

--
-- Generic functions
--

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ []     = return []
mapM f (x:xs) = do y  <- f x
                   ys <- mapM f xs
                   return (y:ys)

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ []     = return []
filterM p (x:xs) = do flag <- p x
                      ys <- filterM p xs
                      return $ if flag then x:ys else ys
