open System
open System.Runtime.InteropServices
open Microsoft.FSharp.Collections


// This is a very basic math interpreter, using unions to discriminate between 
// expression operators.

// An expression is either a constant value, or an arithmetic function applied to
// one or more operands that are expressions.
type Expression =
    | Const of float
    | Add of Expression * Expression
    | Sub of Expression * Expression
    | Neg of Expression
    | Mult of Expression * Expression
    | Div of Expression * Expression
    | Sqrt of Expression
    | Pow of Expression * Expression
    | Input of String
    | Mod of Expression * Expression
    | Var of String
   
// evaluate converts an Expression object into the floating-point number it represents.
let rec evaluate expr state =
    match expr with 
    | Const c -> c
    | Add (expr1, expr2) -> (evaluate expr1 state) + (evaluate expr2 state)
    | Sub (expr1, expr2) -> (evaluate expr1 state) - (evaluate expr2 state)
    | Neg expr1 -> (evaluate expr1 state) * -1.0
    | Mult (expr1, expr2) -> (evaluate expr1 state) * (evaluate expr2 state)
    | Div (expr1, expr2) -> (evaluate expr1 state) / (evaluate expr2 state)
    | Sqrt expr1 -> Math.Sqrt (evaluate expr1 state)
    | Pow (expr1, expr2) -> (evaluate expr1 state) ** (evaluate expr2 state)
    | Input s -> printf "%s" s; Console.ReadLine() |> float
    | Mod (expr1, expr2) -> (evaluate expr1 state) % (evaluate expr2 state)
    | Var v -> state |> Map.find v
    
// format expression
let rec formatExpression expr state=
    match expr with 
    | Const c -> sprintf "%.2f" c
    | Add (expr1, expr2) -> sprintf "(%s + %s)" (formatExpression expr1 state) (formatExpression expr2 state)
    | Sub (expr1, expr2) -> sprintf "(%s - %s)" (formatExpression expr1 state) (formatExpression expr2 state)
    | Neg expr1 -> sprintf "(%s * -1)" (formatExpression expr1 state)
    | Mult (expr1, expr2) -> sprintf "(%s * %s)" (formatExpression expr1 state) (formatExpression expr2 state)
    | Div (expr1, expr2) -> sprintf "(%s / %s)" (formatExpression expr1 state) (formatExpression expr2 state)
    | Sqrt expr1 -> sprintf "(sqrt(%s))" (formatExpression expr1 state)
    | Pow (expr1, expr2) -> sprintf "(%s ** %s)" (formatExpression expr1 state) (formatExpression expr2 state)
    | Input s -> sprintf "%.2f" (float s)
    | Mod (expr1, expr2) -> sprintf "(%s mod %s)" (formatExpression expr1 state) (formatExpression expr2 state)
    | Var v -> sprintf "(%s = %f)" v (state |> Map.find v)
type Condition =
    | Equals of Expression * Expression
    | And of Condition * Condition
    | Or of Condition * Condition
    | Not of Condition

let rec testCondition con state=
    match con with
    | Equals (expr1, expr2) -> evaluate expr1 state = evaluate expr2 state
    | And (con1, con2) -> testCondition con1 state && testCondition con2 state
    | Or (con1, con2) -> testCondition con1 state || testCondition con2 state
    | Not con1 -> testCondition con1 state |> not 
    
type Statement =
    | Noop
    | PrintStr of String
    | PrintExpr of Expression
    | Branch of Condition * Statement * Statement
    | Repeat of int * Statement
    | Set of string * Expression
    //| While of Condition * Statement list
    
let rec interpret state iState=
    match state with
    | Noop -> (); iState
    | PrintStr str -> printfn "%s" str; iState
    | PrintExpr expr -> (evaluate expr iState) |> printfn "%.2f"; iState
    | Branch (con, sList1, sList2) -> if (testCondition con iState) then interpretProgram sList1 iState else interpretProgram sList2 iState |> ignore; iState
    | Repeat (i, sList1) -> if i = 0 then interpret Noop iState else interpretProgram sList1 iState |> ignore; interpret (Repeat (i-1, sList1)) iState |> ignore; iState
    | Set (v, expr) -> iState |> Map.add v (evaluate expr iState)
    //| While (con, sList) -> if (testCondition con iState) then interpretPro; interpret (While (con, sList)) |> ignore; iState

and interpretProgram statementList currentState =
    for item in statementList do
        ans <- interpret item currentState
        
let initialState = Map.ofList [("x", 4.0)]  // hard-code a starting state of x=4.
let myProgram = [
  Set ("x", Add (Const 1.0, Var "x"));
  PrintExpr (Var "x");
  Set ("x", Const 0.0);
  PrintExpr (Var "x")
]
let programResult =
  initialState
  |> interpret (Set ("x", Add (Const 1.0, Var "x")))
  |> interpret (PrintExpr (Var "x"))
  |> interpret (Set ("x", Const 0.0))
  |> interpret (PrintExpr (Var "x"))
  |> printfn "%O"


(*
EVERYTHING BELOW THIS COMMENT WORKS



let initialState = Map.ofList [("x", 4.0)]  // hard-code a starting state of x=4.
let statement = Set ("x", Add (Const 1.0, Var "x"))
interpret statement initialState |> printfn "%O"

let initialState = Map.ofList [("x", 4.0)]  // hard-code a starting state of x=4.
let statement = PrintExpr (Add (Const 1.0, Var "x"))
interpret statement initialState |> printfn "%O"

//let state = Map.ofList [("x", 4.0)]  // hard-code a starting state of x=4.
//let expr = Add (Const 1.0, Var "x")
//evaluate expr state |> printfn "%f"
   
//let cmpFloat = Equals (Const 0.0, Input "Enter a float: ")
//let raisePwer = PrintExpr (Pow ( Input "Enter a base: ", Input "Enter an exponent: "))
//let prog =
//    Branch ( cmpFloat, PrintStr "Good Job", Branch ( Not (cmpFloat), PrintStr "Good Job", Repeat (3, raisePwer)))
//prog
//|> interpret
    
//// Test the input expression
//let x = Console.ReadLine() |> Input
//let demoInput = Add( Const 5.0, x)
//demoInput
//|> evaluate
//|> printfn "%.2f"
//demoInput
//|> formatExpression
//|> printfn "%s"
//    
//// Test the condition union
//let demoCon = Not (Equals( Const 5.0, x))
//demoCon
//|> testCondition
//|> printfn "%b"


// let y = Repeat(3 , PrintStr "Hello World")
// interpret y
// Ugly print the expression.
// let demo1 = Neg (Div (Pow (Const 2.0, Sqrt (Mult (Const 11.0, (Sub (Add (Const 10.0, Const 3.0), Const 2.0))))), Const 2.0))
// printfn "%O\n" demo1

// format and print the expression
// demo1
// |> formatExpression
// |> printfn "%s"

// Evaluate and print the expression.
// demo1
// |> evaluate
// |> printfn "%.2f\n" // prints the result of evaluating 10 + 3 - 2

// a = 1, b = -5, c = 4
// use our "language" to solve the quad formula

printfn("For the quadratic function: x**2 - 5x + 4\nThe answers are: ")
let a = Const 1.0
let b = Const -5.0
let c = Const 4.0
let plus = Div (Add (Neg (b), Sqrt ( Sub (Pow (b, Const 2.0), Mult (Mult (Const 4.0, a), c)))), Mult (Const 2.0, a))
plus
|> formatExpression
|> printfn "%s"
plus
|> evaluate
|> printfn "x = %.2f"

let minus = Div (Sub (Neg (b), Sqrt ( Sub (Pow (b, Const 2.0), Mult (Mult (Const 4.0, a), c)))), Mult (Const 2.0, a))
minus
|> formatExpression
|> printfn "%s"
minus
|> evaluate
|> printfn "x = %.2f"

*)



