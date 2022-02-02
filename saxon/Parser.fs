module saxon.Parser

open saxon.Tokenizer

type Operator =
    | Add
    | Sub
    | Mul
    | Div
    | Exp

type Node =
    | Operation of Node * Operator * Node
    | Number of double
    | Null // todo: fix this when we actually want to implement error handling

let primary (tokens: Token list) =
    match tokens with
    | Token.Number(num) :: tail  -> (Node.Number(num), tail)
    | _ -> (Node.Null, tokens)

let rec exponent (tokens: Token list) =
    let left, tokens = primary(tokens)
    
    match tokens with
    | Token.Exp :: tail ->
        let right, tokens = exponent(tail)
        (Node.Operation(left, Operator.Exp, right), tokens)
    | _ ->
        (left, tokens)
       
let rec product (tokens: Token list) =
    let left, tokens = exponent(tokens)
    
    match tokens with
    | Token.Mul :: tail ->
        let right, tokens = product(tail)
        (Node.Operation(left, Operator.Mul, right), tokens)
    | Token.Div :: tail ->
        let right, tokens = product(tail)
        (Node.Operation(left, Operator.Div, right), tokens)
    | _ ->
        (left, tokens)
        
let rec sum (tokens: Token list) =
    let left, tokens = product(tokens)
    
    match tokens with
    | Token.Add :: tail ->
        let right, tokens = sum(tail)
        (Node.Operation(left, Operator.Add, right), tokens)
    | Token.Sub :: tail ->
        let right, tokens = sum(tail)
        (Node.Operation(left, Operator.Sub, right), tokens)
    | _ ->
        (left, tokens)
        
let rec expression (tokens: Token list) =
    let result, _ = sum tokens
    result