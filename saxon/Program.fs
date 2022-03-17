module Saxon.Program

open System

open saxon.Parser
open saxon.Tokenizer
open saxon.Interpreter
open saxon.Builtins
open saxon.Wrapper


let wrapper = SaxonWrapper()

while true do
    let input = Console.ReadLine()
    let output = wrapper.runInput input
    printfn $"{output}" 
    
   
