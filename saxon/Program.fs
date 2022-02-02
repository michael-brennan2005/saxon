open System
open saxon.Parser
open saxon.Tokenizer

let x = Seq.toList "3.1 + 2.4 ^ 36.23 * 10^-5"
let y = tokenize x []

while true do
    let input = Console.ReadLine()
    let s1 = tokenize (Seq.toList input) []
    let s2 = expression s1
    printfn $"{s2}"

