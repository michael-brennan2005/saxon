module saxon.Interpreter

open System
open Microsoft.FSharp.Collections
open saxon.Parser
type Context = {
    variables: Map<string, Node>
}

let rec walk (node: Node) (context: Context)  =
    match node with
    | Node.Operation(op, lhs, rhs) ->
        let lhsResult, context = walk lhs context
        let rhsResult, context = walk rhs context
        
        match op with
        | Add ->
            (lhsResult + rhsResult, context)
        | Sub ->
            (lhsResult - rhsResult, context)
        | Mul ->
            (lhsResult * rhsResult, context)
        | Div ->
            (lhsResult / rhsResult, context)
        | Exp ->
            (Math.Pow(lhsResult, rhsResult), context)
    | Node.VariableAssignment(identifier, expression) ->
        let result, context = walk expression context
            
        (result, { context with variables = context.variables |> Map.add identifier.name (Node.Number(result)) })
    | Node.Number(num) ->
        (num, context)
    | Node.VariableIdentifier(name) ->
        match context.variables |> Map.find name with
        | Node.Number(result) -> (result, context)
        | _ -> (nan, context)
    | Null ->
        (nan, context)