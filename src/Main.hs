{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Monoid ((<>))

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

instance Foldable Zipper where
  -- foldMap f (Zipper xs y zs) = foldMap f xs <> f y <> foldMap f zs
  foldMap f (Zipper xs y zs) = f y <> foldMap id (zipWith (<>) (map f xs) (map f zs))

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

instance Foldable Grid where
  foldMap f (Grid x) = foldMap id $ fmap (foldMap f) x

newtype SumInt = SumInt Int deriving (Show, Eq, Num)

e :: Monoid m => Grid m
e = Grid $ constZipper $ constZipper mempty

x :: Monoid m => Zipper (Zipper m)
x = fromGrid e

instance Monoid SumInt where
  mempty = SumInt 0
  (SumInt n) `mappend` (SumInt m) = SumInt (n + m)

fromSum :: SumInt -> Int
fromSum (SumInt n) = n

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Ocean Stuff --

avgAdjacent :: Grid Double -> Double
avgAdjacent g = (sumClose g - center) / 8
  where center = extract $ extract $ fromGrid g

sumClose :: Grid Double -> Double
--sumClose = runFold (GenericFoldL id (\(x, n) y -> if n < 2 then x + y else y) 0) 
--sumClose = foldr (\(x, n) y -> if n > 1 then y else x + y) 0
sumClose (Grid (Zipper xs y zs)) = sum [f $ head xs, f y, f $ head zs] where
  f (Zipper xs y zs) = foldl (+) 0 [head xs, y, head zs]

attachGridDist :: Grid a -> Grid (a, Int)
attachGridDist (Grid x) = Grid x3
   where
    x2 = attachDist 0 x
    x3 = fmap (\(x, n) -> attachDist n x) x2

attachDist :: Int -> Zipper a -> Zipper (a, Int)
attachDist base (Zipper xs x ys) = Zipper (zip xs [base + 1..]) (x, base) (zip ys [base + 1..])

stepOcean :: Drivers -> (Grid Double, Int) -> (Grid Double, Int)
stepOcean d (g, t) = (applyDrivers d t (g <<= avgAdjacent), t + 1)

ocillate :: Double -> Int -> Double
ocillate period time = sin (fromIntegral time / period)

type Drivers = [((Int, Int), Double)]

applyDrivers :: Drivers -> Int -> Grid Double -> Grid Double
applyDrivers ds t g = foldl (\grid ((x, y), v) -> putOceanCoord v x y grid) g ps
  where
    ps = map (\((x, y), p) -> ((x, y), ocillate p t)) ds

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Conway Stuff --

boolToInt :: Bool -> SumInt
boolToInt b = if b then SumInt 1 else SumInt 0

neighboursOver :: Foldable f => f Bool -> Int
neighboursOver = fromSum . runFold (GenericFoldL id (\x y -> x + boolToInt y) 0) 

getAdjacent :: Grid Bool -> Int
getAdjacent g = neighboursOver g - neighboursOver (Identity center)
  where center = extract $ extract $ fromGrid g

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

showOcean :: Int -> Grid Double -> String
showOcean n (Grid z) = concat ((\z -> (g <$> toFinite n z) ++ "\n") <$> toFinite n z)
  where
    g x = if x > 0 then 'x' else 'o'

showG :: Show a => Int -> Grid a -> String
showG n (Grid z) = concat $ concat ((\z -> (g <$> toFinite n z) ++ ["\n"]) <$> toFinite n z)
  where
    g x = show x

showGrid :: Int -> Grid Bool -> String
showGrid n (Grid z) = concat ((\z -> (g <$> toFinite n z) ++ "\n") <$> toFinite n z)
  where
    g b = if b then 'x' else 'o'

putCoord :: Int -> Int -> Grid Bool -> Grid Bool
putCoord x y (Grid g) = Grid $ putZip x (\z -> putZip y (\_ -> True) z) g

putOceanCoord :: a -> Int -> Int -> Grid a -> Grid a
putOceanCoord a x y (Grid g) = Grid $ putZip x (\z -> putZip y (\_ -> a) z) g

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

emptyOcean = Grid $ constZipper $ constZipper 0

testOcean :: Grid Double
testOcean =
  --putOceanCoord 0.5  0 (-1) $
  --putOceanCoord 1 0 1    $
  --putOceanCoord (-0.5)  (-1) 0 $
  --putOceanCoord 0  1 0    $
  --putOceanCoord 0 0 0    $
  -- putOceanCoord 0.5  1 (0) $
  -- putOceanCoord 0.5  (-1) (0) $
  emptyOcean

drivers = [((0, 0), 5)]

ox = iterate (stepOcean drivers) (testOcean, 0)
oy = map (showOcean 4 . fst) ox
oz = mapM putStrLn (take 64 oy)

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
