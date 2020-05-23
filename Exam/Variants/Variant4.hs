module Variant4 where

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
findT _               (Leaf x)        = Just x
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
sizeT _ = 0

-- 1 балл
containsT :: Eq a => a -> PrefixTree a -> Bool
containsT  _ _ = False

-- 2 балла
updateT :: a -> IndexT -> PrefixTree a -> PrefixTree a
updateT _ _ _ = Empty

-- 1 балл
reverseT :: PrefixTree a -> PrefixTree a
reverseT _ = Empty

-- 2 балла
foldLeftT :: (b -> a -> b) -> b -> PrefixTree a -> b
foldLeftT _ acc _ = acc

-- 2 балла
dropAfter :: IndexT -> PrefixTree a -> PrefixTree a
dropAfter _ _ = Empty

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
sizeV _ = 0

-- 1 балл
isEmpty :: Vector a -> Bool
isEmpty _ = False

-- 1 балл
lastV :: Vector a -> Maybe a
lastV _ = Nothing

-- 1 балл
containsV :: Eq a => a -> Vector a -> Bool
containsV _ _ = False

-- 2 балла
updateV :: a -> Int -> Vector a -> Vector a
updateV _ _ _ = empty

-- 2 балла
reverseV :: Vector a -> Vector a
reverseV _ = empty

-- 1 балл
foldLeftV :: (b -> a -> b) -> b -> Vector a -> b
foldLeftV _ acc _ = acc

-- 2 балла
dropRight :: Int -> Vector a -> Vector a
dropRight _ _ = empty

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
evalNonEmpty = Right ()

evalHeadV :: Status
evalHeadV = Right ()

evalLastV :: Status
evalLastV = testFun lastV tests "lastV" where
  tests = [(Nothing, empty), (Just 'a', fromList "a"), (Just 'o', fromList "funcpro")]

evalMapV :: Status
evalMapV = Right ()

evalExistsV :: Status
evalExistsV = Right ()

evalForAllV :: Status
evalForAllV = Right ()

evalContainsV :: Status
evalContainsV = testFun (containsV 'e') tests "containsV" where
  tests = [(False, empty), (False, fromList "bdghj"), (True, fromList "abcde")]

evalTailV :: Status
evalTailV = Right ()

evalInitV :: Status
evalInitV = Right ()

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
evalDropV = Right ()

evalTakeV :: Status
evalTakeV = Right ()

evalTakeRight :: Status
evalTakeRight = Right ()

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
