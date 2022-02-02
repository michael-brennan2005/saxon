open System

type Token =
    // Single-char
    | Add
    | Sub
    | Mul
    | Div
    | Exp
    | LeftParen
    | RightParen
    | Equals
    // Keywords
    | Let
    // Variable-length
    | Number of double
    | Identifier of string
    
let rec tokenizeNumber (currentNumber: char list) (str: char list) (toks: Token list) =
    match str with
    | char :: tail when Char.IsDigit(char) || char.Equals('.') -> tokenizeNumber (char :: currentNumber) tail toks
    | [] ->
        let number =
            currentNumber
            |> List.rev
            |> Array.ofList
            |> String.Concat
            |> double
            
        (str, Number(number) :: toks)
    | char :: tail when not (Char.IsDigit(char)) ->
        let number =
            currentNumber
            |> List.rev
            |> Array.ofList
            |> String.Concat
            |> double
            
        (str, Number(number) :: toks)
    
    
let rec tokenizeIdentifier (currentIdentifier: char list) (str: char list) (toks : Token list) =
    match str with
    | [] ->
        let identifier =
            currentIdentifier
            |> List.rev
            |> Array.ofList
            |> string
            
        (str, Identifier(identifier) :: toks)
    | char :: tail when not (Char.IsLetter(char)) ->
        let identifier =
            currentIdentifier
            |> List.rev
            |> Array.ofList
            |> string
        
        (str, Identifier(identifier) :: toks)
    | char :: tail when Char.IsLetter(char) -> tokenizeIdentifier (char :: currentIdentifier) tail toks

let rec tokenize (str: char list) (tokens: Token list) =
    match str with
    | [] -> tokens |> List.rev
    // Single-char
    | '+' :: tail -> tokenize tail (Add :: tokens)
    | '-' :: tail -> tokenize tail (Sub :: tokens)
    | '*' :: tail -> tokenize tail (Mul :: tokens)
    | '/' :: tail -> tokenize tail (Div :: tokens)
    | '^' :: tail -> tokenize tail (Exp :: tokens)
    | '(' :: tail -> tokenize tail (LeftParen :: tokens)
    | ')' :: tail -> tokenize tail (RightParen :: tokens)
    | '=' :: tail -> tokenize tail (Equals :: tokens)
    // Keywords
    | 'l' :: 'e' :: 't' :: char :: tail when not (Char.IsLetter(char)) -> tokenize (char :: tail) (Let :: tokens)
    // Variable-length
    | char :: tail when Char.IsLetter(char) ->
        let (remainingStr, newTokens) = tokenizeIdentifier [] str tokens
        tokenize remainingStr newTokens
    | char :: tail when Char.IsNumber(char) ->
        let (remainingStr, newTokens) = tokenizeNumber [] str tokens
        tokenize remainingStr newTokens
    // Default
    | char :: tail -> tokenize tail tokens 

let rec printTokens (tokens: Token list) =
    match tokens with
    | token :: tail ->
        printfn $"{token}"
        printTokens tail
    | _ -> ()
        
let x = Seq.toList "3.1 + 2.4 ^ 36.23 * 10^-5"
let y = tokenize x []

while true do
    let input = Console.ReadLine()
    printTokens (tokenize (Seq.toList input) [])

