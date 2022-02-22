module saxon.Builtins

open Microsoft.FSharp.Collections
open saxon.Interpreter
open saxon.Parser

let builtinConstants =
    Map.empty
        .Add("pi",  Node.Number(3.14159265359))

let builtinSin (map: Map<string, float>) (context: Context) =
    (sin(Map.find "x" map), context)
    
let builtinNumerical =
    Map.empty
        .Add("sin", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "sin"
                FunctionAssignmentInfo.arguments = ["x"];
            }, builtinSin))
        