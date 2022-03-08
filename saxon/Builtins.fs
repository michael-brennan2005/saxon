module saxon.Builtins

open Microsoft.FSharp.Collections
open saxon.Interpreter
open saxon.Parser

let builtinConstants =
    Map.empty
        .Add("pi",  Node.Number(3.14159265359))
        .Add("e", Node.Number(2.71828182846))
        .Add("tau", Node.Number(6.283185307179586))

let builtinSin (map: Map<string, float>) (context: Context) =
    (sin(Map.find "x" map), context)
    
let builtinCos (map: Map<string, float>) (context: Context) =
    (cos(Map.find "x" map), context)
    
let builtinTan (map: Map<string, float>) (context: Context) =
    (tan(Map.find "x" map), context)
    
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

let builtinTestingFunction (functionContext: Function) (map: Map<string, float>) (context: Context) =
    let node =
        match functionContext with
        | Function.UserDefined(info, node) -> node
        | _ -> Node.Number(0.0)
        
    
    (0.0, context)
    
// Functions that run over possible functional arguments
// Needed because on builtinnumerical and userdefined the compiler does immediate evaluation
let builtinFunctional =
    Map.empty
        .Add("testingfunction", Function.BuiltInFunctional({
            FunctionAssignmentInfo.name = "testingfunction";
            FunctionAssignmentInfo.arguments = ["fx"; "a"; "b";];
        }, builtinTestingFunction))
        
