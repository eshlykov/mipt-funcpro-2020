module Solution where

import Data.Bits (testBit)
import Data.Either (lefts)
import Data.Foldable (foldl')
import Data.List (intercalate)

------------------------------------------------------------------------------------------------------------------------
-- НЕ МЕНЯЙТЕ ЭТУ ЧАСТЬ

data PrefixTree a = Empty
                  | Leaf a
                  | Branch (PrefixTree a) (PrefixTree a)
  deriving (Eq, Show)

type IndexT = [Bool]

findT :: IndexT -> PrefixTree a -> Maybe a
findT _               Empty            = Nothing
findT _               (Leaf x)         = Just x
findT (False : index) (Branch left _)  = findT index left
findT (True : index)  (Branch _ right) = findT index right
findT _               _                = Nothing

insert :: a -> IndexT -> PrefixTree a -> PrefixTree a
insert elem []              _                   = Leaf elem
insert elem (False : index) (Branch left right) = Branch (insert elem index left) right
insert elem (True : index)  (Branch left right) = Branch left (insert elem index right)
insert elem (False : index) tree                = Branch (insert elem index Empty) tree
insert elem (True : index)  tree                = Branch tree (insert elem index Empty)

------------------------------------------------------------------------------------------------------------------------
-- ЗАДАНИЕ 1: ПРЕФИКСНОЕ ДЕРЕВО

-- 1 балл
sizeT :: PrefixTree a -> Int
sizeT Empty               = 0
sizeT (Leaf _)            = 1
sizeT (Branch left right) = sizeT left + sizeT right

-- 1 балл
mapT :: (a -> b) -> PrefixTree a -> PrefixTree b
mapT _ Empty               = Empty
mapT f (Leaf x)            = Leaf (f x)
mapT f (Branch left right) = Branch (mapT f left) (mapT f right)

-- 1 балл
existsT :: (a -> Bool) -> PrefixTree a -> Bool
existsT _    Empty               = False
existsT pred (Leaf x)            = pred x
existsT pred (Branch left right) = existsT pred left || existsT pred right

-- 1 балл
forAllT :: (a -> Bool) -> PrefixTree a -> Bool
forAllT _    Empty               = True
forAllT pred (Leaf x)            = pred x
forAllT pred (Branch left right) = forAllT pred left && forAllT pred right

-- 1 балл
containsT :: Eq a => a -> PrefixTree a -> Bool
containsT _    Empty               = False
containsT elem (Leaf x)            = x == elem
containsT elem (Branch left right) = containsT elem left || containsT elem right

-- 2 балла
remove :: IndexT -> PrefixTree a -> PrefixTree a
remove []              _                   = Empty
remove (False : index) (Branch left right) = Branch (remove index left) right
remove (True: index)   (Branch left right) = Branch left (remove index right)
remove _               tree                = tree

-- 2 балла
updateT :: a -> IndexT -> PrefixTree a -> PrefixTree a
updateT elem []              _                   = Leaf elem
updateT elem (False : index) (Branch left right) = Branch (updateT elem index left) right
updateT elem (True : index)  (Branch left right) = Branch left (updateT elem index right)
updateT _    _               tree                = tree

-- 1 балл
reverseT :: PrefixTree a -> PrefixTree a
reverseT (Branch left right) = Branch (reverseT right) (reverseT left)
reverseT tree                = tree

-- 2 балла
foldLeftT :: (b -> a -> b) -> b -> PrefixTree a -> b
foldLeftT _ acc Empty               = acc
foldLeftT f acc (Leaf x)            = f acc x
foldLeftT f acc (Branch left right) = (inOrder $! inOrder acc right) left
  where
    inOrder acc Empty               = acc
    inOrder acc (Leaf x)            = f acc x
    inOrder acc (Branch left right) = (inOrder $! inOrder acc left) right

-- 2 балла
dropBefore :: IndexT -> PrefixTree a -> PrefixTree a
dropBefore (toRight : index) (Branch left right) | toRight   = Branch left (dropBefore' index right)
                                                 | otherwise = Branch (dropBefore' index left) Empty
  where
    dropBefore' (False : index) (Branch left right) = Branch (dropBefore' index left) right
    dropBefore' (True : index)  (Branch left right) = Branch Empty (dropBefore' index right)
    dropBefore' _               tree                = tree
dropBefore _               tree                = tree

-- 2 балла
dropAfter :: IndexT -> PrefixTree a -> PrefixTree a
dropAfter (toRight : index) (Branch left right) | toRight   = Branch Empty (dropAfter' index right)
                                                | otherwise = Branch (dropAfter' index left) right
  where
    dropAfter' (False : index) (Branch left right) = Branch (dropAfter' index left) Empty
    dropAfter' (True : index)  (Branch left right) = Branch left (dropAfter' index right)
    dropAfter' _               tree                = tree
dropAfter _               tree                = tree

------------------------------------------------------------------------------------------------------------------------
-- НЕ МЕНЯЙТЕ ЭТУ ЧАСТЬ

data Vector a = Vector Int Int (PrefixTree a)
  deriving (Eq, Show)

empty :: Vector a
empty = Vector 0 0 Empty

toIndexT :: Int -> IndexT
toIndexT int = map (testBit int) [31, 30 .. 0]

findV :: Int -> Vector a -> Maybe a
findV index (Vector from _ tree) = findT (toIndexT (from + index)) tree

prepended :: a -> Vector a -> Vector a
prepended elem (Vector from until tree) = Vector (from - 1) until (insert elem (toIndexT (from - 1)) tree)

appended :: a -> Vector a -> Vector a
appended elem (Vector from until tree) = Vector from (until + 1) (insert elem (toIndexT until) tree)

fromList :: [a] -> Vector a
fromList = foldl' (flip appended) empty

toList :: Vector a -> [a]
toList vector @ (Vector from until _) = maybe [] id $ sequenceA $ map (flip findV vector) [0 .. until - from - 1]

------------------------------------------------------------------------------------------------------------------------
-- ЗАДАНИЕ 2: ВЕКТОР

-- 1 балл
sizeV :: Vector a -> Int
sizeV (Vector from until _) = until - from

-- 1 балл
isEmpty :: Vector a -> Bool
isEmpty vector = sizeV vector == 0

-- 1 балл
nonEmpty :: Vector a -> Bool
nonEmpty vector = sizeV vector > 0

-- 1 балл
headV :: Vector a -> Maybe a
headV = findV 0

-- 1 балл
lastV :: Vector a -> Maybe a
lastV vector = findV (sizeV vector - 1) vector

-- 1 балл
mapV :: (a -> b) -> Vector a -> Vector b
mapV f (Vector from until tree) = Vector from until (mapT f tree)

-- 1 балл
existsV :: (a -> Bool) -> Vector a -> Bool
existsV pred (Vector _ _ tree) = existsT pred tree

-- 1 балл
forAllV :: (a -> Bool) -> Vector a -> Bool
forAllV pred (Vector _ _ tree) = forAllT pred tree

-- 1 балл
containsV :: Eq a => a -> Vector a -> Bool
containsV elem (Vector _ _ tree) = containsT elem tree

-- 2 балла
tailV :: Vector a -> Vector a
tailV vector @ (Vector from until tree) =
  if isEmpty vector then empty
  else Vector (from + 1) until (remove (toIndexT from) tree)

-- 2 балла
initV :: Vector a -> Vector a
initV vector @ (Vector from until tree) =
  if isEmpty vector then empty
  else Vector from (until - 1) (remove (toIndexT (until - 1)) tree)

-- 2 балла
updateV :: a -> Int -> Vector a -> Vector a
updateV elem index vector @ (Vector from until tree) =
  if isEmpty vector then empty
  else Vector from until (updateT elem (toIndexT (from + index)) tree)

-- 2 балла
reverseV :: Vector a -> Vector a
reverseV (Vector from until tree) = Vector (-until) (-from) (reverseT tree)

-- 1 балл
foldLeftV :: (b -> a -> b) -> b -> Vector a -> b
foldLeftV f acc (Vector _ _ tree) = foldLeftT f acc tree

-- 2 балла
dropV :: Int -> Vector a -> Vector a
dropV count vector @ (Vector from until tree) =
  if count <= 0 then vector
  else if count >= sizeV vector then empty
  else Vector (from + count) until (dropBefore (toIndexT (from + count)) tree)

-- 2 балла
takeV :: Int -> Vector a -> Vector a
takeV count vector @ (Vector from _ tree) =
  if count <= 0 then empty
  else if count >= sizeV vector then vector
  else Vector from (from + count) (dropAfter (toIndexT (from + count - 1)) tree)

-- 2 балла
takeRight :: Int -> Vector a -> Vector a
takeRight count vector @ (Vector _ until tree) =
  if count <= 0 then empty
  else if count >= sizeV vector then vector
  else Vector (until - count) until (dropBefore (toIndexT (until - count)) tree)

-- 2 балла
dropRight :: Int -> Vector a -> Vector a
dropRight count vector @ (Vector from until tree) =
  if count <= 0 then vector
  else if count >= sizeV vector then empty
  else Vector from (until - count) (dropAfter (toIndexT (until - count - 1)) tree)

------------------------------------------------------------------------------------------------------------------------
-- НЕ МЕНЯЙТЕ ЭТУ ЧАСТЬ

type Status = Either String ()

validate :: Eq a => [(a, a)] -> String -> Status
validate results message =
  case all (\ (expected, actual) -> expected == actual) results of
    True  -> Right ()
    False -> Left message

calculate :: (Vector a -> b) -> [(b, Vector a)] -> [(b, b)]
calculate f = map (fmap f)

testFun :: Eq b => (Vector a -> b) -> [(b, Vector a)] -> String -> Status
testFun f tests = validate (calculate f tests)

------------------------------------------------------------------------------------------------------------------------
-- НЕ МЕНЯЙТЕ ЭТУ ЧАСТЬ

evalSizeV :: Status
evalSizeV = testFun sizeV tests "sizeV" where
  tests = [(0, empty), (1, prepended 'a' empty), (1, appended 'z' empty), (10, fromList "abcdefghij")]

evalIsEmpty :: Status
evalIsEmpty = testFun isEmpty tests "isEmpty" where
  tests = [(True, empty), (False, prepended 'a' empty), (False, appended 'z' empty), (False, fromList "abcd")]

evalNonEmpty :: Status
evalNonEmpty = testFun nonEmpty tests "nonEmpty" where
  tests = [(False, empty), (True, prepended 'a' empty), (True, appended 'z' empty), (True, fromList "abcd")]

evalHeadV :: Status
evalHeadV = testFun headV tests "headV" where
  tests = [(Nothing, empty), (Just 'a', fromList "a"), (Just 'f', fromList "funcpro")]

evalLastV :: Status
evalLastV = testFun lastV tests "lastV" where
  tests = [(Nothing, empty), (Just 'a', fromList "a"), (Just 'o', fromList "funcpro")]

evalMapV :: Status
evalMapV = testFun (mapV (succ . succ . succ)) tests "mapV" where
  tests = [(empty, empty), (fromList "defg", fromList "abcd")]

evalExistsV :: Status
evalExistsV = testFun (existsV (== 'e')) tests "existsV" where
  tests = [(False, empty), (False, fromList "bdghj"), (True, fromList "abcde")]

evalForAllV :: Status
evalForAllV = testFun (forAllV (== 'e')) tests "forAllV" where
  tests = [(True, empty), (True, fromList "eee"), (False, fromList "e-oo-eee")]

evalContainsV :: Status
evalContainsV = testFun (containsV 'e') tests "containsV" where
  tests = [(False, empty), (False, fromList "bdghj"), (True, fromList "abcde")]

evalTailV :: Status
evalTailV = testFun (toList . tailV) tests "tailV" where
  tests = [("", empty), ("", fromList "a"), ("bcd", fromList "abcd")]

evalInitV :: Status
evalInitV = testFun (toList . initV) tests "initV" where
  tests = [("", empty), ("", fromList "a"), ("abc", fromList "abcd")]

evalUpdateV :: Status
evalUpdateV = testFun (updateV 'c' 3) tests "updateV" where
  tests = [(fromList "funcpro", fromList "fundpro")]

evalReverseV :: Status
evalReverseV = testFun (toList . reverseV) tests "reverseV" where
  tests = [("", empty), ("ba", prepended 'a' $ appended 'b' empty)]

evalfoldLeftV :: Status
evalfoldLeftV = testFun (foldLeftV (flip (:)) []) tests "foldLeftV" where
  tests = [([], empty), ([1], fromList [1]), ([4, 3 .. 1], fromList [1 .. 4])]

evalDropV :: Status
evalDropV = testFun (toList . dropV 2) tests "dropV" where
  tests = [("", empty), ("", fromList "a"), ("", fromList "ab"), ("c", fromList "abc"), ("ncpro", fromList "funcpro")]

evalTakeV :: Status
evalTakeV = testFun (toList . takeV 2) tests "takeV" where
  tests = [("", empty), ("a", fromList "a"), ("ab", fromList "ab"), ("ab", fromList "abc"), ("fu", fromList "funcpro")]

evalTakeRight :: Status
evalTakeRight = testFun (toList . takeRight 2) tests "takeRight" where
  tests = [("", empty), ("a", fromList "a"), ("ab", fromList "ab"), ("bc", fromList "abc"), ("ro", fromList "funcpro")]

evalDropRight :: Status
evalDropRight = testFun (toList . dropRight 2) tests "dropRight" where
  tests = [("", empty), ("", fromList "a"), ("", fromList "ab"), ("a", fromList "abc"), ("funcp", fromList "funcpro")]

------------------------------------------------------------------------------------------------------------------------
-- НЕ МЕНЯЙТЕ ЭТУ ЧАСТЬ

main :: IO ()
main = do
  let statuses = [evalSizeV, evalIsEmpty, evalNonEmpty, evalHeadV, evalLastV, evalMapV, evalExistsV, evalForAllV,
                  evalContainsV, evalTailV, evalInitV, evalUpdateV, evalReverseV, evalfoldLeftV, evalDropV, evalTakeV,
                  evalTakeRight, evalDropRight]
  case lefts statuses of
    [] -> putStrLn "Success"
    ms -> putStrLn $ "Failure: " ++ intercalate ", " ms
