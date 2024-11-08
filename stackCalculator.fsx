(*
    A stack based calculator used to calculate the sum of the first n natural numbers, n(n + 1)/2.
    - n represents the number of terms (or the highest number) in the series.
    - (n+1) represents the next number after n.
    - Multiplying n by (n+1) gives the product of the two consecutive numbers.
    - Dividing the product by 2 gives the sum of the numbers
*)


// ==============================================
// Types
// ==============================================

type Stack = StackContents of float list

// ==============================================
// Stack primitives
// ==============================================

/// Push a value on the stack
let push x (StackContents contents) = StackContents(x :: contents)

/// Pop a value from the stack and return it
/// and the new stack as a tuple
let pop (StackContents contents) =
    match contents with
    | top :: rest ->
        let newStack = StackContents rest
        (top, newStack)
    | [] -> failwith "Stack underflow"

// ==============================================
// Operator core
// ==============================================

// pop the top two elements
// do a binary operation on them
// push the result
let binary mathFn stack =
    let y, stack' = pop stack
    let x, stack'' = pop stack'
    let z = mathFn x y
    push z stack''

// pop the top element
// do a unary operation on it
// push the result
let unary f stack =
    let x, stack' = pop stack
    push (f x) stack'

// ==============================================
// Other core
// ==============================================

/// Pop and show the top value on the stack
let SHOW stack =
    let x, _ = pop stack
    printfn $"The answer is %f{x}"
    stack // keep going with same stack

/// Duplicate the top value on the stack
let DUP stack =
    let x, s = pop stack
    push x (push x s)

/// Swap the top two values
let SWAP stack =
    let x, s = pop stack
    let y, s' = pop s
    push y (push x s')

/// Drop the top value on the stack
let DROP stack =
    let _, s = pop stack //pop the top of the stack
    s //return the rest

// ==============================================
// Words based on primitives
// ==============================================

// Constants
// -------------------------------
let EMPTY = StackContents []
let START = EMPTY


// Numbers
// -------------------------------
let ONE = push 1.0
let TWO = push 2.0
let THREE = push 3.0
let FOUR = push 4.0
let FIVE = push 5.0


// Math functions
// -------------------------------
let ADD = binary (+)
let SUB = binary (-)
let MUL = binary (*)
let DIV = binary (/)

let NEG = unary (fun x -> -x)


// ==============================================
// Words based on composition
// ==============================================

let SQUARE = DUP >> MUL

let CUBE = DUP >> DUP >> MUL >> MUL

let SUM_NUMBERS_UPTO =
    DUP // n, n           2 items on stack
    >> ONE // n, n, 1        3 items on stack
    >> ADD // n, (n+1)       2 items on stack
    >> MUL // n(n+1)         1 item on stack
    >> TWO // n(n+1), 2      2 items on stack
    >> DIV // n(n+1)/2       1 item on stack
