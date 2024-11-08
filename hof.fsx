(*
    Demonstrates the following functional programming idioms:
    - The use of custom higher order functions
    - The use of built-in higher order functions
    - The use of partial function applications and currying
*)


let drive distance petrol =
    if distance > 50 then petrol / 2.0
    elif distance > 25 then petrol - 10.0
    elif distance > 0 then petrol - 1.
    else petrol

let startPetrol = 100.0

startPetrol |> drive 60 |> drive 30 |> drive 10


// ===============================================
// Tupling curried functions
// ===============================================

// f is a curried function but can be called in a tupled fashion using uncurry
let uncurry f (x, y) = f x y

let rec range i j =
    match i > j with
    | true -> []
    | false -> i :: range (i + 1) j

let countup = uncurry range

let listOfSeven = countup (1, 7)


// ===============================================
// Currying tupled functions
// ===============================================

// f is a tupled function but can be called in a curried fashion using curry
let curry f x y = f (x, y)

let rec tupledRange (i, j) =
    match i > j with
    | true -> []
    | false -> i :: tupledRange ((i + 1), j)

// call tupledRange in a curried fashion
let countupTo = curry tupledRange 1

let sevenItems = countupTo 7


let rec map f xs =
    match xs with
    | [] -> []
    | x :: xs' -> f x :: map f xs'

let rec filter f xs =
    match xs with
    | [] -> []
    | x :: xs' ->
        match f x with
        | true -> x :: filter f xs'
        | false -> filter f xs'

// fold polyfill
let rec reduceLeft f acc xs =
    match xs with
        | [] -> acc
        | x :: xs' -> reduceLeft f (f x acc) xs'

// foldBack polyfill implemented to start folding from the right
let rec reduceRight f acc xs =
    match xs with
        | [] -> acc
        | x :: xs' -> f (reduceRight f acc xs') x
            
// execute f, n times
let rec nTimes f n x =
    match n with
    | 0 -> x
    | _ -> f (nTimes f (n - 1) x)

// representation of an arithmetic expression
type exp =
    | Constant of int
    | Negate of exp
    | Add of exp * exp
    | Multiply of exp * exp

let rec eval e =
    match e with
    | Constant i -> i
    | Negate e -> -(eval e)
    | Add(e1, e2) -> (eval e1) + (eval e2)
    | Multiply(e1, e2) -> (eval e1) * (eval e2)

let rec trueOfAllConstants predicate exp =
    match exp with
    | Constant i -> predicate i
    | Negate expr -> trueOfAllConstants predicate expr
    | Add(expr1, expr2) -> trueOfAllConstants predicate expr1 && trueOfAllConstants predicate expr2
    | Multiply(expr1, expr2) -> trueOfAllConstants predicate expr1 && trueOfAllConstants predicate expr2

let inline sumFold input = (0, input) |> Seq.fold (+)

// Assuming that the list is non-empty
[ 1..10 ] |> Seq.reduce (+)

[ 1..10 ] |> List.scan (+) 0
