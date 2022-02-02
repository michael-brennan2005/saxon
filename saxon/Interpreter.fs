module saxon.Interpreter

open System
open saxon.Parser
let rec walk (node: Node) =
    match node with
    | Node.Operation(op, lhs, rhs) ->
        let lhsResult = walk lhs
        let rhsResult = walk rhs
        
        match op with
        | Add ->
            lhsResult + rhsResult
        | Sub ->
            lhsResult - rhsResult
        | Mul ->
            lhsResult * rhsResult
        | Div ->
            lhsResult / rhsResult
        | Exp ->
            Math.Pow(lhsResult, rhsResult)
    | Node.Number(num) ->
        num
    | Null ->
        nan
        