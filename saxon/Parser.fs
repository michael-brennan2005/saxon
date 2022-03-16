// "rec"-ing namespace is a code smell. but who cares
module rec saxon.Parser

open saxon.Tokenizer

(*
Selectors

insideParens: Returns the tokens inside parentheses, assumes left paren was already used up
    
Grammar
parse ->
    expression;
    variableAssignment;
    functionAssignment;

variableAssignment ->
    "let" variableDefinition "=" expression;
variableDefinition ->
    IDENTIFIER;  
    
functionAssignment ->
    "let" functionDefinition "=" expression;
functionDefinition ->
    IDENTIFIER "(" functionDefinition;
    IDENTIFIER;
    "," IDENTIFIER;

expression -> 
    sum;
sum -> 
    product;
    product ("+" | "-") sum;
product ->
    exponentOrUnary;
    exponentOrUnary ("*" | "/") product;
exponentOrUnary ->
    "-" exponentOrUnary;
    primary "^" exponentOrUnary;
    primary;
primary ->
    NUMBER;
    IDENTIFIER;
    IDENTIFIER "(" functionCallArguments ")"
    "(" expression ")";    
*)

type Operator =
    | Add
    | Mul
    | Exp

type VariableAssignmentInfo = {
    name: string
}

type FunctionAssignmentInfo = {
    name: string
    arguments: string list 
}

type Node =
    | Operation of Operator * Node * Node
    | Inverse of Node
    | Negate of Node
    | VariableAssignment of VariableAssignmentInfo * Node
    | FunctionAssignment of FunctionAssignmentInfo * Node
    | Number of float
    | VariableCall of string
    | FunctionCall of string * Node list // node list is arguments

// MARK: Selectors

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
      
let rec splitAtEquals (leftTokens: Token list) (remainingTokens: Token list) =
    match remainingTokens with
    | Token.Equals :: tail -> (leftTokens |> List.rev, tail)
    | token :: tail -> splitAtEquals (token :: leftTokens) tail
    | _ -> (leftTokens, [])
    
let rec splitAtCommas (splitTokens: Token list list) (currentList: Token list) (remainingTokens: Token list) =
    match remainingTokens with
    | Token.Comma :: tail -> splitAtCommas ((currentList |> List.rev) :: splitTokens) [] tail
    | token :: tail -> splitAtCommas splitTokens (token :: currentList) tail
    | [] -> currentList :: splitTokens |> List.rev
    
// MARK: Expression parsing
let rec functionCallArguments (arguments: Node list) (tokens: Token list) =
    match tokens with
    | [] -> arguments |> List.rev
    | Token.Comma :: tail -> functionCallArguments arguments tail
    | _ ->
        let argument, remainingTokens = expressionWithTokens tokens
        functionCallArguments (argument :: arguments) remainingTokens
        
let primary (tokens: Token list) =
    match tokens with
    | Token.Number(num) :: tail  -> (Node.Number(num), tail)
    | Token.Identifier(name) :: Token.LeftParen :: tail ->
        let insideTokens, remainingTokens = insideParens [] tail 1 0
        let arguments = functionCallArguments [] insideTokens
        (Node.FunctionCall(name, arguments), remainingTokens)
    | Token.Identifier(name) :: tail -> (Node.VariableCall(name), tail)
    | Token.LeftParen :: tail ->
        let insideTokens, remainingTokens = insideParens [] tail 1 0
        (expression insideTokens, remainingTokens)
    | _ -> (Node.Number(0.0), tokens)

let rec exponentOrUnary (tokens: Token list) =
    match tokens with
    | Token.Sub :: tail ->
        let right, tokens = exponentOrUnary(tail)
        (Node.Negate(right), tokens)
    | _ ->
        let left, tokens = primary(tokens)
        
        match tokens with
        | Token.Exp :: tail ->
            let right, tokens = exponentOrUnary(tail)
            (Node.Operation(Operator.Exp, left, right), tokens)
        | _ ->
            (left, tokens)

// Helper for product
let inverse (node: Node) =
    match node with
    | Node.Operation(op, left, right) -> Node.Operation(op, Node.Inverse(left), right)
    | Node.Negate(node) -> Node.Inverse(Node.Negate(node))
    | Node.Inverse(node) -> node
    | Node.Number(num) -> Node.Number(1.0 / num)
    | Node.VariableCall(name) -> Node.Inverse(Node.VariableCall(name))
    | Node.FunctionCall(name, argList) -> Node.Inverse(Node.FunctionCall(name, argList))
    | _  -> Node.Number(0.0)
    
let rec product (tokens: Token list) =
    let left, tokens = exponentOrUnary(tokens)
    
    match tokens with
    | Token.Mul :: tail ->
        let right, tokens = product(tail)
        (Node.Operation(Operator.Mul, left, right), tokens)
    | Token.Div :: tail ->
        let right, tokens = product(tail)
        (Node.Operation(Operator.Mul, left, inverse right), tokens)
    | _ ->
        (left, tokens)
        
// Helper for sum
let negate (node: Node) =
    match node with
    | Node.Operation(op, left, right) -> Node.Operation(op, Node.Negate(left), right)
    | Node.Negate(node) -> node
    | Node.Inverse(node) -> Node.Negate(Node.Inverse(node))
    | Node.Number(num) -> Node.Number(0.0 - num)
    | Node.VariableCall(name) -> Node.Negate(Node.VariableCall(name))
    | Node.FunctionCall(name, argList) -> Node.Negate(Node.FunctionCall(name, argList))
    | _  -> Node.Number(0.0)
    
let rec sum (tokens: Token list) =
    let left, tokens = product(tokens)
    
    match tokens with
    | Token.Add :: tail ->
        let right, tokens = sum(tail)
        (Node.Operation(Operator.Add, left, right), tokens)
    | Token.Sub :: tail ->
        let right, tokens = sum(tail)
        (Node.Operation(Operator.Add, left, negate right), tokens)
    | _ ->
        (left, tokens)
        
let expressionWithTokens (tokens: Token list) =
    sum tokens
    
let expression (tokens: Token list) =
    let result, _ = sum tokens
    result
   

// MARK: Assignment parsing
let rec variableDefinition (tokens: Token list) =
    match tokens with
    | [Token.Identifier(name)] -> { VariableAssignmentInfo.name = name }
    | _ -> { VariableAssignmentInfo.name = "nildef" }
   
let rec functionDefinition (functionName: string) (argList: string list) (tokens: Token list) =
    match tokens with
    | Token.Identifier(functionName) :: Token.LeftParen :: tail -> functionDefinition functionName [] tail
    | Token.Identifier(argName) :: tail -> functionDefinition functionName (argName :: argList) tail
    | Token.Comma :: tail -> functionDefinition functionName argList tail
    | _ -> { FunctionAssignmentInfo.arguments = argList |> List.rev; FunctionAssignmentInfo.name = functionName }
    
let variableAssignment (tokens: Token list) =
    let leftSide, rightSide = splitAtEquals [] tokens
    
    Node.VariableAssignment(variableDefinition leftSide, expression rightSide)
    
let functionAssignment (tokens: Token list) =
    let leftSide, rightSide = splitAtEquals [] tokens
    
    Node.FunctionAssignment(functionDefinition "" [] leftSide, expression rightSide)
   
// MARK: Main
let parse (tokens: Token list) =
    match tokens with
    | Token.Let :: Token.Identifier(name) :: Token.LeftParen ::  tail -> functionAssignment (Token.Identifier(name) :: Token.LeftParen :: tail)
    | Token.Let :: tail -> variableAssignment tail
    | _ -> expression tokens