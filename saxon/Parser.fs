// "rec"-ing namespace is a code smell. but who cares
module rec saxon.Parser

open saxon.Tokenizer

type Operator =
    | Add
    | Sub
    | Mul
    | Div
    | Exp

type Node =
    | Operation of Operator * Node * Node
    | Number of float
    | Null // todo: fix this when we actually want to implement error handling

// assumes left paren was already consumed
let rec insideParens (insideTokens: Token list) (remainingTokens: Token list) (lp: int) (rp: int) =
    match remainingTokens with
    | Token.LeftParen :: tail ->
        insideParens ( Token.LeftParen :: insideTokens ) tail (lp + 1) rp
    | Token.RightParen :: tail when rp + 1 = lp ->
        (insideTokens |> List.rev, tail)
    | Token.RightParen :: tail ->
        insideParens (Token.RightParen :: insideTokens ) tail lp (rp + 1)
    | [] ->
        (insideTokens |> List.rev, remainingTokens)
    | token :: tail ->
        insideParens (token :: insideTokens) tail lp rp
      
let primary (tokens: Token list) =
    match tokens with
    | Token.Number(num) :: tail  -> (Node.Number(num), tail)
    | Token.LeftParen :: tail ->
        let (insideTokens, remainingTokens) = insideParens [] tail 1 0
        (expression insideTokens, remainingTokens)
    | _ -> (Node.Null, tokens)

let rec exponentOrUnary (tokens: Token list) =
    match tokens with
    | Token.Sub :: tail ->
        let right, tokens = exponentOrUnary(tail)
        (Node.Operation(Operator.Sub, Node.Number(0.0), right), tokens)
    | _ ->
        let left, tokens = primary(tokens)
        
        match tokens with
        | Token.Exp :: tail ->
            let right, tokens = exponentOrUnary(tail)
            (Node.Operation(Operator.Exp, left, right), tokens)
        | _ ->
            (left, tokens)

let rec product (tokens: Token list) =
    let left, tokens = exponentOrUnary(tokens)
    
    match tokens with
    | Token.Mul :: tail ->
        let right, tokens = product(tail)
        (Node.Operation(Operator.Mul, left, right), tokens)
    | Token.Div :: tail ->
        let right, tokens = product(tail)
        (Node.Operation(Operator.Div, left, right), tokens)
    | _ ->
        (left, tokens)
        
let rec sum (tokens: Token list) =
    let left, tokens = product(tokens)
    
    match tokens with
    | Token.Add :: tail ->
        let right, tokens = sum(tail)
        (Node.Operation(Operator.Add, left, right), tokens)
    | Token.Sub :: tail ->
        let right, tokens = sum(tail)
        (Node.Operation(Operator.Sub, left, right), tokens)
    | _ ->
        (left, tokens)
        
let rec expression (tokens: Token list) =
    let result, _ = sum tokens
    result