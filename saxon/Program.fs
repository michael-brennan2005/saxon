// CLI for saxon.
module Saxon.Program

open System
open saxon.Wrapper


let wrapper = SaxonWrapper()

printfn "Welcome to Saxon!"

while true do
    printf "sxn> "
    let input = Console.ReadLine()
    let output = wrapper.runInput input
    printfn $"-> {output}" 
    
   
