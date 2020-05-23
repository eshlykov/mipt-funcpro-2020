﻿module Variant3

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// НЕ МЕНЯЙТЕ ЭТУ ЧАСТЬ

let testBit int index = int &&& (1 <<< index) <> 0

let flip f x y = f y x

let sequenceA list = if List.contains None list then None else Some (List.map Option.get list)

let maybe fallBack f = function
    | Some x -> f x
    | None -> fallBack

let fmap f (y, x) = (y, f x)

type Status = Left of string | Right

let succ c = char (int c + 1)

let lefts list =
    let isLeft = function | Left _ -> true | _ -> false
    let fromLeft = function | Left message -> message | _ -> ""
    list |> List.filter isLeft |> List.map fromLeft

let intercalate delim strings = List.reduce (fun acc str -> acc + delim + str) strings

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// НЕ МЕНЯЙТЕ ЭТУ ЧАСТЬ

type 'a PrefixTree =
    | Empty
    | Leaf of 'a
    | Branch of 'a PrefixTree * 'a PrefixTree

let rec findT index tree =
    match index, tree with
    | _, Empty -> None
    | _, Leaf x -> Some x
    | false :: index', Branch (left, _) -> findT index' left
    | true :: index', Branch (_, right) -> findT index' right
    | _ -> None

let rec insert elem index tree =
    match index, tree with
    | [], _ -> Leaf elem
    | false :: index', Branch (left, right) -> Branch (insert elem index' left, right)
    | true :: index', Branch (left, right) -> Branch (left, insert elem index' right)
    | false :: index', tree -> Branch (insert elem index' Empty, tree)
    | true :: index', tree -> Branch (tree, insert elem index' Empty)

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// ЗАДАНИЕ 1: ПРЕФИКСНОЕ ДЕРЕВО

// 1 балл
// sizeT : 'a PrefixTree -> int
let rec sizeT = function _ -> 0

// 1 балл
// forAllT : ('a -> bool) -> 'a PrefixTree -> bool
let rec forAllT _ = function _ -> false

// 2 балла
// updateT : 'a -> bool list -> 'a PrefixTree -> 'a PrefixTree
let rec updateT _ _ _ = Empty

// 1 балл
// reverseT : 'a PrefixTree -> 'a PrefixTree
let rec reverseT = function _ -> Empty

// 2 балла
// foldLeftT : ('b -> 'a -> 'b) -> 'b -> 'a PrefixTree -> 'b
let foldLeftT _ acc _ = acc

// 2 балла
// dropBefore : bool list -> 'a PrefixTree -> 'a PrefixTree
let dropBefore _ _ = Empty

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// НЕ МЕНЯЙТЕ ЭТУ ЧАСТЬ

type 'a Vector = Vector of int * int * 'a PrefixTree

let empty = Vector (0, 0, Empty)

let toIndexT int = List.map (testBit int) [31 .. -1 .. 0]

let findV index (Vector (from, _, tree)) = findT (toIndexT (from + index)) tree

let prepended elem (Vector (from, until, tree)) = Vector (from - 1, until, insert elem (toIndexT (from - 1)) tree)

let appended elem (Vector (from, until, tree)) = Vector (from, until + 1, insert elem (toIndexT until) tree)

let fromList list = List.fold (flip appended) empty list

let toList (Vector (from, until, _) as vector) =
    maybe [] id (sequenceA (List.map (flip findV vector) [0 .. until - from - 1]))

let fromString string = fromList [for c in string -> c]

let toString vector = toList vector |> List.fold (fun s c -> s + string c) ""

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// ЗАДАНИЕ 2: ВЕКТОР

// 1 балл
let sizeV _ = 0

// 1 балл
let nonEmpty _ = false

// 1 балл
let lastV _ = None

// 1 балл
let forAllV _ _ = false

// 2 балла
let updateV _ _ _ = empty

// 2 балла
let reverseV _ = empty

// 1 балл
let foldLeftV _ acc _ = acc

// 2 балла
let takeRight _ _ = empty

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// НЕ МЕНЯЙТЕ ЭТУ ЧАСТЬ

let validate results message =
    match List.forall (function (expected, desired) -> expected = desired) results with
    | true -> Right
    | false -> Left message

let calculate f list = List.map (fmap f) list

let testFun f tests message = validate (calculate f tests) message

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// НЕ МЕНЯЙТЕ ЭТУ ЧАСТЬ

let evalSizeV =
    let tests =  [0, empty; 1, prepended 'a' empty; 1, appended 'z' empty; 10, fromString "abcdefghij"]
    testFun sizeV tests "sizeV"

let evalIsEmpty = Right

let evalNonEmpty =
    let tests = [false, empty; true, prepended 'a' empty; true, appended 'z' empty; true, fromString "abcd"]
    testFun nonEmpty tests "nonEmpty"

let evalHeadV = Right

let evalLastV =
    let tests = [None, empty; Some 'a', fromString "a"; Some 'o', fromString "funcpro"]
    testFun lastV tests "lastV"

let evalMapV = Right

let evalExistsV = Right

let evalForAllV =
    let tests = [true, empty; true, fromString "eee"; false, fromString "e-oo-eee"]
    testFun (forAllV (( = ) 'e')) tests "forAllV"

let evalContainsV = Right

let evalTailV = Right

let evalInitV = Right

let evalUpdateV =
    let tests = [fromString "funcpro", fromString "fundpro"]
    testFun (updateV 'c' 3) tests "updateV"

let evalReverseV =
    let tests = ["", empty; "ba", prepended 'a' <| appended 'b' empty]
    testFun (reverseV >> toString) tests "reverseV"

let evalfoldLeftV =
    let tests = [[], empty; [1], fromList [1]; [4 .. -1 .. 1], fromList [1 .. 4]]
    testFun (foldLeftV (fun xs x -> x :: xs) []) tests "foldLeftV"

let evalDropV = Right

let evalTakeV = Right

let evalTakeRight =
    let tests = ["", empty; "a", fromString "a"; "ab", fromString "ab"; "bc", fromString "abc"
                 "ro", fromString "funcpro"]
    testFun (takeRight 2 >> toString) tests "takeRight"

let evalDropRight = Right

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// НЕ МЕНЯЙТЕ ЭТУ ЧАСТЬ

[<EntryPoint>]
let main _ =
    let statuses = [evalSizeV; evalIsEmpty; evalNonEmpty; evalHeadV; evalLastV; evalMapV; evalExistsV; evalForAllV
                    evalContainsV; evalTailV; evalInitV; evalUpdateV; evalReverseV; evalfoldLeftV; evalDropV; evalTakeV
                    evalTakeRight; evalDropRight]
    match lefts statuses with
    | [] -> printfn "Success"
    | ms -> printfn "Failure: %s" <| intercalate ", " ms
    0
