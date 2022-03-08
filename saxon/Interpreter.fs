module rec saxon.Interpreter

open System
open Microsoft.FSharp.Collections
open saxon.Parser

type Context = {
    variables: Map<string, Node>
    functions: Map<string, Function>
}

type Function =
    | UserDefined of FunctionAssignmentInfo * Node
    | BuiltinNumerical of FunctionAssignmentInfo * (Map<string, float> -> Context -> float * Context)
    | BuiltInFunctional of FunctionAssignmentInfo * (string -> Map<string, float> -> Context -> float * Context)
    
let rec mapZip (left: 'a list) (right: 'b list) (map: Map<'a, 'b>) =
    match (left, right) with
    | headLeft :: tailLeft, headRight :: tailRight -> mapZip tailLeft tailRight (map |> Map.add headLeft headRight)
    | _ -> map
    
let rec mapMerge (top: Map<'a, 'b>) (bottom: Map<'a, 'b>) =
    Map.fold (fun acc key value -> Map.add key value acc) bottom top

let rec evalFunction (toEval: Function) (arguments: Node list) (context: Context) =
    match toEval with
    | Function.BuiltinNumerical(info, func) ->
        let argumentsEvaluated =
            arguments
            |> List.map (fun arg ->
                let result, _ = walk arg context
                result)
        let (formalToReal: Map<string, float>) =
            mapZip info.arguments argumentsEvaluated Map.empty
        func formalToReal context
    | Function.BuiltInFunctional(info, func) ->
        let functionName =
            match arguments.Head with
            // IT IS NOT A VARIABLE CALL! Parser will say it is though, but we know its actually the name of a function.
            | Node.VariableCall(string) -> string
            | _ -> "err"
        let functionContext = context.functions |> Map.find functionName
       
        let argumentsEvaluated =
            arguments.Tail
            |> List.map (fun arg ->
                let result, _ = walk arg context
                result) 
        let (formalToReal: Map<string, float>) =
            mapZip info.arguments.Tail argumentsEvaluated Map.empty
        func functionContext formalToReal context
    | Function.UserDefined(info, node) ->
        let argumentsEvaluated =
            arguments
            |> List.map (fun arg ->
                let result, _ = walk arg context
                Node.Number(result))
        let (formalToReal: Map<string, Node>) =
            mapZip info.arguments argumentsEvaluated Map.empty
        let newContext = { context with variables = mapMerge formalToReal context.variables }
        let result, _ = walk node newContext
        (result, context)
                
let rec walk (node: Node) (context: Context)  =
    match node with
    | Node.Operation(op, left, right) ->
        let leftResult, context = walk left context
        let rightResult, context = walk right context
        
        match op with
        | Add -> (leftResult + rightResult, context)
        | Sub -> (leftResult - rightResult, context)
        | Mul -> (leftResult * rightResult, context)
        | Div -> (leftResult / rightResult, context)
        | Exp -> (Math.Pow(leftResult,rightResult), context)
    | Node.VariableAssignment(info, node) ->
        (0.0, { context with variables = context.variables |> Map.add info.name node })
    | Node.FunctionAssignment(info, node) ->
        (0.0, {
        context with functions = context.functions.Add (info.name, Function.UserDefined(info, node)) 
    })
    | Node.Number(value) ->
        (value, context)
    | Node.VariableCall(name) ->
        let node = context.variables |> Map.find name
        let result, context = walk node context
        (result, context)
    | Node.FunctionCall(name, arguments) ->
        let fn = (context.functions |> Map.find name)
        evalFunction fn arguments context
    | Node.Null -> (nan, context)
    