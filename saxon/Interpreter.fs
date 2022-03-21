// AST -> Float and a new context.
module rec saxon.Interpreter

open System
open Microsoft.FSharp.Collections
open saxon.Parser
    
type Context = {
    variables: Map<string, Node>
    functions: Map<string, Function>
    message: string option
}

let findVariable (context: Context) (name: string)  =
    context.variables |> Map.find name
    
let findFunction (context: Context) (name: string) =
    context.functions |> Map.find name
    
type Function =
    | UserDefined of FunctionAssignmentInfo * Node
    | BuiltinNumerical of FunctionAssignmentInfo * (Context -> float * Context)
    | BuiltInFunctional of FunctionAssignmentInfo * (Function -> Context -> float * Context)
    
let rec mapZip (left: 'a list) (right: 'b list) (map: Map<'a, 'b>) =
    match (left, right) with
    | headLeft :: tailLeft, headRight :: tailRight -> mapZip tailLeft tailRight (map |> Map.add headLeft headRight)
    | _ -> map
    
let rec mapMerge (top: Map<'a, 'b>) (bottom: Map<'a, 'b>) =
    Map.fold (fun acc key value -> Map.add key value acc) bottom top

// Evaluates arguments and inserts them into the context.
let rec evalArgumentsAndCreateNewContext (functionInfo: FunctionAssignmentInfo) (arguments: Node list) (context: Context) =
    let argumentsEvaluated =
            arguments
            |> List.map (fun arg ->
                let result, _ = walk arg context
                Node.Number(result))
    let (formalToReal: Map<string, Node>) =
            mapZip functionInfo.arguments argumentsEvaluated Map.empty
    let newContext = { context with variables = mapMerge formalToReal context.variables }
    newContext
    
// Evaluates the function.
let rec evalFunction (toEval: Function) (arguments: Node list) (context: Context) =
    match toEval with
    | Function.BuiltinNumerical(info, func) ->
        let newContext = evalArgumentsAndCreateNewContext info arguments context
        func newContext
    | Function.BuiltInFunctional(info, func) ->
        let functionName =
            match arguments.Head with
            // IT IS NOT A VARIABLE CALL! Parser will say it is though, but we know its actually the name of a function.
            | Node.VariableCall(string) -> string
            | _ -> "err"
        let functionContext = context.functions |> Map.find functionName
        // Tail because the first argument is the function!
        let newContext = evalArgumentsAndCreateNewContext {info with arguments = info.arguments.Tail} arguments.Tail context
        func functionContext newContext
    | Function.UserDefined(info, node) ->
        let newContext = evalArgumentsAndCreateNewContext info arguments context
        let result, _ = walk node newContext
        (result, context)
               
let rec walk (node: Node) (context: Context) : float * Context =
    match node with
    | Node.Operation(op, left, right) ->
        let leftResult, context = walk left context
        let rightResult, context = walk right context
        
        match op with
        | Add -> (leftResult + rightResult, context)
        | Mul -> (leftResult * rightResult, context)
        | Exp -> (Math.Pow(leftResult,rightResult), context)
    | Node.Parentheses(node) ->
        walk node context
    | Node.Inverse(node) ->
        let result, context = walk node context
        (1.0 / result, context)
    | Node.Negate(node) ->
        let result, context = walk node context
        (0.0 - result, context)
    | Node.VariableAssignment(info, node) ->
        (0.0, { context with variables = context.variables |> Map.add info.name node; message = Some "Variable successfully created." })
    | Node.FunctionAssignment(info, node) ->
        (0.0, {
        context with functions = context.functions.Add (info.name, Function.UserDefined(info, node)); message = Some "Function successfully created."
    })
    | Node.Number(value) ->
        (value, context)
    | Node.VariableCall(name) ->
        let node = findVariable context name
        let result, context = walk node context
        (result, context)
    | Node.FunctionCall(name, arguments) ->
        let fn = findFunction context name
        evalFunction fn arguments context
    