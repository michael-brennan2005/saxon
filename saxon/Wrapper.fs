// Interface for saxon. Wraps all the passes into a nice interface.
module saxon.Wrapper

(* For use with saxon-ui and saxon-tests. THE BOUNDARY. *)
open saxon.Parser
open saxon.Tokenizer
open saxon.Interpreter
open saxon.Builtins
   
type SaxonWrapper() =
    let mutable context = {
        Context.message = None
        Context.variables = builtinConstants
        Context.functions = mapMerge builtinNumerical builtinFunctional
    }
    
    member this.runInput (input: string) =
        let s1 = tokenize (Seq.toList input) []
        let s2 = parse s1
        let s3, newContext = walk s2 context
        context <- newContext
        
        match context.message with
        | Some(string) ->
            context <- {context with message = None }
            string
        | _ ->
             $"{s3}"
       