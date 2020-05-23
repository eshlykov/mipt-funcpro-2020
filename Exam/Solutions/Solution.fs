module Solution

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
let rec sizeT = function
    | Empty -> 0
    | Leaf _ -> 1
    | Branch (left, right) -> sizeT left + sizeT right

// 1 балл
let rec mapT f = function
    | Empty -> Empty
    | Leaf x -> Leaf (f x)
    | Branch (left, right) -> Branch (mapT f left, mapT f right)

// 1 балл
let rec existsT pred = function
    | Empty -> false
    | Leaf x -> pred x
    | Branch (left, right) -> existsT pred left || existsT pred right

// 1 балл
let rec forAllT pred = function
    | Empty -> true
    | Leaf x -> pred x
    | Branch (left, right) -> forAllT pred left && forAllT pred right

// 1 балл
let rec containsT elem = function
    | Empty -> false
    | Leaf x -> x = elem
    | Branch (left, right) -> containsT elem left || containsT elem right

// 2 балла
let rec remove index tree =
    match index, tree with
    | [], _ -> Empty
    | false :: index', Branch (left, right) -> Branch (remove index' left, right)
    | true :: index', Branch (left, right) -> Branch (left, remove index' right)
    | _, tree -> tree

// 2 балла
let rec updateT elem index tree =
    match index, tree with
    | [], _ -> Leaf elem
    | false :: index', Branch (left, right) -> Branch (updateT elem index' left, right)
    | true :: index', Branch (left, right) -> Branch (left, updateT elem index' right)
    | _, tree -> tree

// 1 балл
let rec reverseT = function
    | Branch (left, right) -> Branch (reverseT right, reverseT left)
    | tree -> tree

// 2 балла
let foldLeftT f acc tree =
    let rec inOrder acc = function
        | Empty -> acc
        | Leaf x -> f acc x
        | Branch (left, right) -> inOrder (inOrder acc left) right
    match tree with
    | Empty -> acc
    | Leaf x -> f acc x
    | Branch (left, right) -> inOrder (inOrder acc right) left

// 2 балла
let dropBefore index tree =
    let rec dropBefore' index tree =
        match index, tree with
            | false :: index', Branch (left, right) -> Branch (dropBefore' index' left, right)
            | true :: index', Branch (_, right) -> Branch (Empty, dropBefore' index' right)
            | _, tree -> tree
    match index, tree with
    | false :: index', Branch (left, _) -> Branch (dropBefore' index' left, Empty)
    | true :: index', Branch (left, right) -> Branch (left, dropBefore' index' right)
    | _, tree -> tree

// 2 балла
let dropAfter index tree =
    let rec dropAfter' index tree =
        match index, tree with
        | false :: index', Branch (left, _) -> Branch (dropAfter' index' left, Empty)
        | true :: index', Branch (left, right) -> Branch (left, dropAfter' index' right)
        | _, tree -> tree
    match index, tree with
    | false :: index', Branch (left, right) -> Branch (dropAfter' index' left, right)
    | true :: index', Branch (_, right) -> Branch (Empty, dropAfter' index' right)
    | _, tree -> tree

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
let sizeV (Vector (from, until, _)) = until - from

// 1 балл
let isEmpty vector = sizeV vector = 0

// 1 балл
let nonEmpty vector = sizeV vector <> 0

// 1 балл
let headV vector = findV 0 vector

// 1 балл
let lastV vector = findV (sizeV vector - 1) vector

// 1 балл
let mapV f (Vector (from, until, tree)) = Vector (from, until, mapT f tree)

// 1 балл
let existsV pred (Vector (_, _, tree)) = existsT pred tree

// 1 балл
let forAllV pred (Vector (_, _, tree)) = forAllT pred tree

// 1 балл
let containsV elem (Vector (_, _, tree)) = containsT elem tree

// 2 балла
let tailV (Vector (from, until, tree) as vector) =
    if isEmpty vector then empty
    else Vector (from + 1, until, remove (toIndexT from) tree)

// 2 балла
let initV (Vector (from, until, tree) as vector) =
    if isEmpty vector then empty
    else Vector (from, until - 1, remove (toIndexT (until - 1)) tree)

// 2 балла
let updateV elem index (Vector (from, until, tree) as vector) =
    if isEmpty vector then empty
    else Vector (from, until, updateT elem (toIndexT (from + index)) tree)

// 2 балла
let reverseV (Vector (from, until, tree)) = Vector (-until, -from, reverseT tree)

// 1 балл
let foldLeftV f acc (Vector (_, _, tree)) = foldLeftT f acc tree

// 2 балла
let dropV count (Vector (from, until, tree) as vector) =
    if count <= 0 then vector
    else if count >= sizeV vector then empty
    else Vector (from + count, until, dropBefore (toIndexT (from + count)) tree)

// 2 балла
let takeV count (Vector (from, _, tree) as vector) =
    if count <= 0 then empty
    else if count >= sizeV vector then vector
    else Vector (from, from + count, dropAfter (toIndexT (from + count - 1)) tree)

// 2 балла
let takeRight count (Vector (_, until, tree) as vector) =
    if count <= 0 then empty
    else if count >= sizeV vector then vector
    else Vector (until - count, until, dropBefore (toIndexT (until - count)) tree)

// 2 балла
let dropRight count (Vector (from, until, tree) as vector) =
    if count <= 0 then vector
    else if count >= sizeV vector then empty
    else Vector (from, until - count, dropAfter (toIndexT (until - count - 1)) tree)

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

let evalIsEmpty =
    let tests = [true, empty; false, prepended 'a' empty; false, appended 'z' empty; false, fromString "abcd"]
    testFun isEmpty tests "isEmpty"

let evalNonEmpty =
    let tests = [false, empty; true, prepended 'a' empty; true, appended 'z' empty; true, fromString "abcd"]
    testFun nonEmpty tests "nonEmpty"

let evalHeadV =
    let tests = [None, empty; Some 'a', fromString "a"; Some 'f', fromString "funcpro"]
    testFun headV tests "headV"

let evalLastV =
    let tests = [None, empty; Some 'a', fromString "a"; Some 'o', fromString "funcpro"]
    testFun lastV tests "lastV"

let evalMapV =
    let tests = [empty, empty; fromString "defg", fromString "abcd"]
    testFun (mapV (succ >> succ >> succ)) tests "mapV"

let evalExistsV =
    let tests = [false, empty; false, fromString "bdghj"; true, fromString "abcde"]
    testFun (existsV (( = ) 'e')) tests "existsV"

let evalForAllV =
    let tests = [true, empty; true, fromString "eee"; false, fromString "e-oo-eee"]
    testFun (forAllV (( = ) 'e')) tests "forAllV"

let evalContainsV =
    let tests = [false, empty; false, fromString "bdghj"; true, fromString "abcde"]
    testFun (containsV 'e') tests "containsV"

let evalTailV =
    let tests = ["", empty; "", fromString "a"; "bcd", fromString "abcd"]
    testFun (tailV >> toString) tests "tailV"

let evalInitV =
    let tests = ["", empty; "", fromString "a"; "abc", fromString "abcd"]
    testFun (initV >> toString) tests "initV"

let evalUpdateV =
    let tests = [fromString "funcpro", fromString "fundpro"]
    testFun (updateV 'c' 3) tests "updateV"

let evalReverseV =
    let tests = ["", empty; "ba", prepended 'a' <| appended 'b' empty]
    testFun (reverseV >> toString) tests "reverseV"

let evalfoldLeftV =
    let tests = [[], empty; [1], fromList [1]; [4 .. -1 .. 1], fromList [1 .. 4]]
    testFun (foldLeftV (fun xs x -> x :: xs) []) tests "foldLeftV"

let evalDropV =
    let tests = ["", empty; "", fromString "a"; "", fromString "ab"; "c", fromString "abc"
                 "ncpro", fromString "funcpro"]
    testFun (dropV 2 >> toString) tests "dropV"

let evalTakeV =
    let tests = ["", empty; "a", fromString "a"; "ab", fromString "ab"; "ab", fromString "abc"
                 "fu", fromString "funcpro"]
    testFun (takeV 2 >> toString) tests "takeV"

let evalTakeRight =
    let tests = ["", empty; "a", fromString "a"; "ab", fromString "ab"; "bc", fromString "abc"
                 "ro", fromString "funcpro"]
    testFun (takeRight 2 >> toString) tests "takeRight"

let evalDropRight =
    let tests = ["", empty; "", fromString "a"; "", fromString "ab"; "a", fromString "abc"
                 "funcp", fromString "funcpro"]
    testFun (dropRight 2 >> toString) tests "dropRight"

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
