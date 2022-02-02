open System
open saxon.Tokenizer

let x = Seq.toList "3.1 + 2.4 ^ 36.23 * 10^-5"
let y = tokenize x []

while true do
    let input = Console.ReadLine()
    printTokens (tokenize (Seq.toList input) [])

