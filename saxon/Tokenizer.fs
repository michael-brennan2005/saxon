module saxon.Tokenizer

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
    | Comma
    | Equals
    // Keywords
    | Let
    | LetF
    // Variable-length
    | Number of float
    | Identifier of string
    
let rec tokenizeNumber (currentNumber: char list) (str: char list) (toks: Token list) =
    match str with
    | char :: tail when Char.IsDigit(char) || char.Equals('.') -> tokenizeNumber (char :: currentNumber) tail toks
    | _ ->
        let number =
            currentNumber
            |> List.rev
            |> Array.ofList
            |> String.Concat
            |> float
            
        (str, Number(number) :: toks)
    
    
let rec tokenizeIdentifier (currentIdentifier: char list) (str: char list) (toks : Token list) =
    match str with
     | char :: tail when Char.IsLetter(char) -> tokenizeIdentifier (char :: currentIdentifier) tail toks
     | _ ->
        let identifier =
            currentIdentifier
            |> List.rev
            |> Array.ofList
            |> String.Concat
            
        (str, Identifier(identifier) :: toks)

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
    | ',' :: tail -> tokenize tail (Comma :: tokens)
    | '=' :: tail -> tokenize tail (Equals :: tokens)
    // Keywords
    | 'l' :: 'e' :: 't' :: 'f' :: char :: tail when not (Char.IsLetter(char)) -> tokenize (char :: tail) (LetF :: tokens)
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