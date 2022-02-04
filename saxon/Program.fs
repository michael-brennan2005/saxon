open System

open saxon.Parser
open saxon.Tokenizer
open saxon.Interpreter

let x = Seq.toList "3.1 + 2.4 ^ 36.23 * 10^-5"
let y = tokenize x []

while true do
    let input = Console.ReadLine()
    let result =
        tokenize (Seq.toList input) []
        |> parse
    printfn $"{result}"

