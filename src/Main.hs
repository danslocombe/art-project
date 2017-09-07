{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

newtype Identity a = Identity a

instance Foldable Identity where
  foldl f zero (Identity x) = f zero x
  foldr f zero (Identity x) = f x zero

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Comonad Stuff

class Copointed m where
  extract :: m a -> a

class (Functor m, Copointed m) => Comonad m where
  duplicate :: m a -> m (m a)

(<<=) :: Comonad m => m a -> (m a -> b) -> m b
(<<=) x f = fmap f (duplicate x)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Profunctor Stuff

class Profunctor f where
  lmap :: (c -> a) -> f a b -> f c b
  rmap :: (b -> d) -> f a b -> f a d

data GenericFoldL a b = forall s. GenericFoldL (s -> b) (s -> a -> s) s

instance Profunctor GenericFoldL where
  lmap f (GenericFoldL result iterate zero) = GenericFoldL result (\x y -> iterate x (f y)) zero
  rmap f (GenericFoldL result iterate zero) = GenericFoldL (f . result) iterate zero

runFold :: Foldable f => GenericFoldL a b -> f a -> b
runFold (GenericFoldL result iterate zero) = result . foldl iterate zero

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Zipper Stuff --

data Zipper a = Zipper [a] a [a] 
  deriving (Functor, Show, Eq)

left :: Zipper a -> Zipper a
left (Zipper xs y (z:zs)) = Zipper (y:xs) z zs

right :: Zipper a -> Zipper a
right (Zipper (x:xs) y zs) = Zipper xs x (y:zs)

instance Copointed Zipper where
  extract (Zipper _ x _) = x

instance Comonad Zipper where
  duplicate x = Zipper (iterate left x) x (iterate right x)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Grid Stuff --

newtype Grid a = Grid (Zipper (Zipper a))
  deriving (Functor, Show, Eq)

fromGrid :: Grid a -> (Zipper (Zipper a))
fromGrid (Grid x) = x

instance Copointed Grid where
  extract = extract . extract . fromGrid

instance Comonad Grid where
  duplicate (Grid x) = fmap Grid $ Grid $ oneUp $ oneUp x
    where
      dropIter f = tail . iterate f
      oneUp :: Zipper (Zipper a) -> Zipper (Zipper (Zipper a))
      oneUp a = Zipper (dropIter (fmap left) a) a (dropIter (fmap right) a)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Conway Stuff --

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

neighboursOver :: Foldable f => f Bool -> Int
neighboursOver = runFold (GenericFoldL id (\x y -> x + boolToInt y) 0) 

getAdjacent :: Grid Bool -> Int
getAdjacent (Grid (Zipper xs y zs)) = (f $ head xs) + f y + (f $ head zs) - (neighboursOver (Identity $ extract y))
  where
    f (Zipper xs y zs) = neighboursOver [head xs, y, head zs]

aliveNext :: Grid Bool -> Bool
aliveNext g = let n = getAdjacent g in
  if extract g
    then case n of
      x | x < 2 -> False
      x | x < 4 -> True
      _         -> False
    else n == 3

step :: Grid Bool -> Grid Bool
step = (<<= aliveNext)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Drawing Stuff --

toFinite :: Int -> Zipper a -> [a]
toFinite n (Zipper xs y zs) = reverse (take n xs) ++ [y] ++ (take n zs)

showGrid :: Int -> Grid Bool -> String
showGrid n (Grid z) = concat ((\z -> (g <$> toFinite n z) ++ "\n") <$> toFinite n z)
  where
    g b = if b then 'x' else 'o'

putCoord :: Int -> Int -> Grid Bool -> Grid Bool
putCoord x y (Grid g) = Grid $ putZip x (\z -> putZip y (\_ -> True) z) g

putZip :: Int -> (a -> a) -> Zipper a -> Zipper a
putZip 0 f (Zipper xs y zs) = Zipper xs (f y) zs
putZip n f (Zipper xs y zs) = Zipper xs' y zs'
  where
    zs' = if n > 0 then [a' | (i, a) <- zip [1..] zs, let a' = if i ==  n then f a else a] else zs
    xs' = if n < 0 then [a' | (i, a) <- zip [1..] xs, let a' = if i == -n then f a else a] else xs

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

constZipper :: a -> Zipper a
constZipper x = Zipper (repeat x) x (repeat x)

emptyLine = constZipper False
emptyGrid = Grid $ constZipper emptyLine

testMap :: Grid Bool
testMap =
  putCoord 0 (-1) $
  putCoord 0 1    $
  putCoord (-1) 0 $
  putCoord 1 0    $
  putCoord 0 0    $
  emptyGrid

printIters :: Grid Bool -> Int -> IO ()
printIters init n = do
  let p = putStrLn . (showGrid 3)
      maps = take n $ iterate step init
  mapM_ p maps

main :: IO ()
main = do
  printIters testMap 10
