module Saxon.Program

open System

open saxon.Parser
open saxon.Tokenizer
open saxon.Interpreter
open saxon.Builtins


let createContext = {
    Context.variables = builtinConstants
    Context.functions = mapMerge builtinNumerical builtinFunctional
}
    
let computeFromInput (context: Context) (input: string) =
    let s1 = tokenize (Seq.toList input) []
    let s2 = parse s1
    walk s2 context
    
let mutable context = createContext
while true do
    let input = Console.ReadLine()
    
    let result, newContext = computeFromInput context input 
    context <- newContext
    
    printfn $"{result}"

