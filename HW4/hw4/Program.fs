
open System

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
   
// evaluate converts an Expression object into the floating-point number it represents.
let rec evaluate expr =
    match expr with 
    | Const c -> c
    | Add (expr1, expr2) -> (evaluate expr1) + (evaluate expr2)
    | Sub (expr1, expr2) -> (evaluate expr1) - (evaluate expr2)
    | Neg expr1 -> (evaluate expr1) * -1.0
    | Mult (expr1, expr2) -> (evaluate expr1) * (evaluate expr2)
    | Div (expr1, expr2) -> (evaluate expr1) / (evaluate expr2)
    | Sqrt expr1 -> Math.Sqrt (evaluate expr1)
    | Pow (expr1, expr2) -> (evaluate expr1) ** (evaluate expr2)
    | Input s -> printf "%s" s; Console.ReadLine() |> float

// format expression
let rec formatExpression expr =
    match expr with 
    | Const c -> sprintf "%.2f" c
    | Add (expr1, expr2) -> sprintf "(%s + %s)" (formatExpression expr1) (formatExpression expr2)
    | Sub (expr1, expr2) -> sprintf "(%s - %s)" (formatExpression expr1) (formatExpression expr2)
    | Neg expr1 -> sprintf "(%s * -1)" (formatExpression expr1)
    | Mult (expr1, expr2) -> sprintf "(%s * %s)" (formatExpression expr1) (formatExpression expr2)
    | Div (expr1, expr2) -> sprintf "(%s / %s)" (formatExpression expr1) (formatExpression expr2)
    | Sqrt expr1 -> sprintf "(sqrt(%s))" (formatExpression expr1)
    | Pow (expr1, expr2) -> sprintf "(%s ** %s)" (formatExpression expr1) (formatExpression expr2)
    | Input s -> sprintf "%.2f" (float s)

type Condition =
    | Equals of Expression * Expression
    | And of Condition * Condition
    | Or of Condition * Condition
    | Not of Condition

let rec testConditon con =
    match con with
    | Equals (expr1, expr2) -> evaluate expr1 = evaluate expr2
    | And (con1, con2) -> testConditon con1 && testConditon con2
    | Or (con1, con2) -> testConditon con1 || testConditon con2
    | Not con1 -> testConditon con1 |> not
    
type Statement =
    | Noop
    | PrintStr of String
    | PrintExpr of Expression
    | Branch of Condition * Statement * Statement
    | Repeat of int * Statement
    
let rec interpret state =
    match state with
    | Noop -> ()
    | PrintStr str -> printfn "%s" str
    | PrintExpr expr -> evaluate expr |> printfn "%.2f"
    | Branch (con, s1, s2) -> if (testConditon con) then interpret s1 else interpret s2
    | Repeat (i, s1) -> if i = 0 then interpret Noop else interpret s1; interpret (Repeat (i-1, s1) )
    
let cmpFloat = Equals (Const 0.0, Input "Enter a float: ")
let raisePwer = PrintExpr (Pow ( Input "Enter a base: ", Input "Enter an exponent: "))
let prog =
    Branch ( cmpFloat, PrintStr "Good Job", Branch ( Not (cmpFloat), PrintStr "Good Job", Repeat (3, raisePwer)))
prog
|> interpret
    
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
//|> testConditon
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

(*printfn("For the quadratic function: x**2 - 5x + 4\nThe answers are: ")
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
|> printfn "x = %.2f"*)



