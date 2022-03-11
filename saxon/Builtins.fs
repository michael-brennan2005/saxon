module saxon.Builtins

open System
open Microsoft.FSharp.Collections
open saxon.Interpreter
open saxon.Parser

let builtinConstants =
    Map.empty
        .Add("pi",  Node.Number(3.14159265359))
        .Add("e", Node.Number(2.71828182846))
        .Add("tau", Node.Number(6.283185307179586))

let builtinSin (context: Context) =
    let result, _ = walk (findVariable context "x") context 
    (sin(result), context)
    
let builtinCos (context: Context) =
    let result, _ = walk (findVariable context "x") context
    (cos(result), context)
    
let builtinTan (context: Context) =
    let result, _ = walk (findVariable context "x") context     
    (tan(result), context)
    
// Functions that run over purely numerical arguments.
let builtinNumerical =
    Map.empty
        .Add("sin", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "sin";
                FunctionAssignmentInfo.arguments = ["x"];
            }, builtinSin))
        .Add("cos", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "cos";
                FunctionAssignmentInfo.arguments = ["x"];
            }, builtinCos))
        .Add("tan", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "tan";
                FunctionAssignmentInfo.arguments = ["x"];
            }, builtinTan))

let builtinDerive (functionContext: Function) (context: Context) =
    // difference quotient calculation
    let h = 0.000000001
    let aPlusH =
        match findVariable context "a" with
        | Node.Number(x) -> x + h
        | _ -> h
    
    
    let ahR, _ = evalFunction functionContext [ Node.Number(aPlusH) ] context
    let aR, _ = evalFunction functionContext [ findVariable context "a" ] context
    ((ahR - aR) / h, context)
    
// Functions that run over possible functional arguments
// Needed because on builtinnumerical and userdefined the compiler does immediate evaluation
let builtinFunctional =
    Map.empty
        .Add("derive", Function.BuiltInFunctional({
            FunctionAssignmentInfo.name = "test";
            FunctionAssignmentInfo.arguments = ["fx"; "a";];
        }, builtinDerive))
        
