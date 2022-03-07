open System

open saxon.Parser
open saxon.Tokenizer
open saxon.Interpreter
open saxon.Builtins

let x = Seq.toList "3.1 + 2.4 ^ 36.23 * 10^-5"
let y = tokenize x []

let mutable context = {
    Context.variables = builtinConstants
    Context.functions = mapMerge builtinNumerical builtinFunctional
}

while true do
    let input = Console.ReadLine()
 
    let s1 = tokenize (Seq.toList input) []
    let s2 = parse s1
    let s3, newContext = walk s2 context
    context <- newContext
    
    printfn $"{s3}"

