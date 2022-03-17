module saxon.Builtins

open Microsoft.FSharp.Collections
open saxon.Interpreter
open saxon.Parser

let rec printTree_ (string: string option) (node: Node)  =
    let printTree (node_: Node) = printTree_ None node_
    
    let rec printArgs (args: Node list) =
        match args with
        | arg :: [] -> $"{printTree arg}"
        | arg :: tail -> $"{printTree arg},{printArgs tail}"
        | _ -> ""
    
    match node with
    | Node.Operation(op, left, right) ->
        match op with
        | Operator.Add ->
            match right with
            | Node.Negate(negateNode) -> $"({printTree left}) - {printTree negateNode}"
            | _ -> $"({printTree left}) + {printTree right}"
        | Operator.Mul ->
            match right with
            | Node.Inverse(inverseNode) -> $"{printTree left} / {printTree inverseNode}"
            | _ -> $"{printTree left} * {printTree right}"
        | Operator.Exp -> $"{printTree left} ^ {printTree right}"
    | Node.Inverse(node) -> $"1 / {printTree node}"
    | Node.Negate(node) -> $"1 - {printTree node}"
    | Node.Number(float) -> $"{float}"
    | Node.VariableCall(string) -> $"{string}"
    | Node.FunctionCall(string, args) -> $"{string}({printArgs args})"
    | _ -> ""

// i am running out of names.
let printFunction (functionC: Function) = 
    match functionC with
    | Function.UserDefined(functionAssignmentInfo, node) -> printTree_ None node
    | _ -> ""
    
let builtinConstants =
    Map.empty
        .Add("pi",  Node.Number(3.14159265359))
        .Add("e", Node.Number(2.71828182846))
        .Add("tau", Node.Number(6.283185307179586))

let builtinSin (context: Context) =
    let result, _ = walk (findVariable context "x") context 
    (sin(result), context)
    
let builtinCos (context: Context) =
    let result, _ = walk (findVariable context "x") context
    (cos(result), context)
    
let builtinTan (context: Context) =
    let result, _ = walk (findVariable context "x") context     
    (tan(result), context)
    
let builtinCsc (context: Context) =
    let result, _ = walk (findVariable context "x") context     
    (1.0 / sin(result), context)
    
let builtinSec (context: Context) =
    let result, _ = walk (findVariable context "x") context     
    (1.0 / cos(result), context)
    
let builtinCot (context: Context) =
    let result, _ = walk (findVariable context "x") context     
    (1.0 / tan(result), context)

let builtinArccos (context: Context) =
    let result, _ = walk (findVariable context "x") context
    (acos(result), context)
    
let builtinArcsin (context: Context) =
    let result, _ = walk (findVariable context "x") context
    (asin(result), context)

let builtinArctan (context: Context) =
    let result, _ = walk (findVariable context "x") context
    (atan(result), context)
    
let builtinLn (context: Context) =
    let result, _ = walk (findVariable context "x") context
    (log(result), context)

let builtinLog (context: Context) =
    let x, _ = walk (findVariable context "x") context
    let y, _ = walk (findVariable context "y") context    
    (log(y) / log(x), context)
    
let rec gcd (a: float) (b: float) =
    if a < b then
        gcd b a
    else if a % b = 0 then
        b
    else 
        gcd b (a % b)

let builtinGcd (context: Context) =
    let x, _ = walk (findVariable context "x") context
    let y, _ = walk (findVariable context "y") context  
    (gcd x y, context)
    
let builtinLcm (context: Context) =
    let x, _ = walk (findVariable context "x") context
    let y, _ = walk (findVariable context "y") context  
    ((x * y) / gcd x y, context)
    
let builtinMod (context: Context) =
    let x, _ = walk (findVariable context "x") context
    let y, _ = walk (findVariable context "y") context  
    (x % y, context)

let builtinAbs (context: Context) =
    let x, _ = walk (findVariable context "x") context
    (abs(x), context)
    
let builtinSqrt (context: Context) =
    let x, _ = walk (findVariable context "x") context
    (sqrt(x), context)

let builtinCbrt (context: Context) =
    let x, _ = walk (findVariable context "x") context
    (x ** (1.0 / 3.0), context)

let builtinRoot (context: Context) =
    let x, _ = walk (findVariable context "x") context
    let y, _ = walk (findVariable context "y") context
    (y ** (1.0 / x), context)

// Functions that run over purely numerical arguments.
let builtinNumerical =
    Map.empty
        // TRIG
        .Add("sin", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "sin";
                FunctionAssignmentInfo.arguments = ["x"];
            }, builtinSin))
        .Add("cos", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "cos";
                FunctionAssignmentInfo.arguments = ["x"];
            }, builtinCos))
        .Add("tan", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "tan";
                FunctionAssignmentInfo.arguments = ["x"];
            }, builtinTan))
        .Add("csc", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "csc";
                FunctionAssignmentInfo.arguments = ["x"];
            }, builtinCsc))
        .Add("sec", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "sec";
                FunctionAssignmentInfo.arguments = ["x"];
            }, builtinSec))
        .Add("cot", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "cot";
                FunctionAssignmentInfo.arguments = ["x"];
            }, builtinCot))
        
        // INVERSE TRIG
        .Add("arccos", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "arccos";
                FunctionAssignmentInfo.arguments = ["x"];
            }, builtinArccos))
        .Add("arcsin", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "arcsin";
                FunctionAssignmentInfo.arguments = ["x"];
            }, builtinArcsin))
        .Add("arctan", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "arctan";
                FunctionAssignmentInfo.arguments = ["x"];
            }, builtinArctan))
        
        // LOGARITHMIC
        .Add("ln", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "ln";
                FunctionAssignmentInfo.arguments = ["x"];
            }, builtinLn))
        .Add("log", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "log";
                FunctionAssignmentInfo.arguments = ["x"; "y";];
            }, builtinLog))
       
       // NUMBER
       .Add("lcm", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "lcm";
                FunctionAssignmentInfo.arguments = ["x"; "y";];
            }, builtinLcm))
        .Add("gcd", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "gcd";
                FunctionAssignmentInfo.arguments = ["x"; "y";];
            }, builtinGcd))
        .Add("mod", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "mod";
                FunctionAssignmentInfo.arguments = ["x"; "y";];
            }, builtinMod))
        .Add("abs", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "abs"
                FunctionAssignmentInfo.arguments = ["x"];
            }, builtinAbs))
        
        // ROOTS
        .Add("sqrt", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "sqrt";
                FunctionAssignmentInfo.arguments = ["x"];
            }, builtinSqrt))
        .Add("cbrt", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "cbrt";
                FunctionAssignmentInfo.arguments = ["x"];
            }, builtinCbrt))
        .Add("root", Function.BuiltinNumerical({
                FunctionAssignmentInfo.name = "mod";
                FunctionAssignmentInfo.arguments = ["x"; "y";];
            }, builtinRoot))

let rec symbolicDerive (functionArg: Node) (context: Context) =
    (0.0, context)
    
let numericallyDerive (functionArg: Function) (context: Context) =
    // difference quotient calculation
    let h = 0.000000001
    let aPlusH =
        match findVariable context "a" with
        | Node.Number(x) -> x + h
        | _ -> h
    
    
    let ahR, _ = evalFunction functionArg [ Node.Number(aPlusH) ] context
    let aR, _ = evalFunction functionArg [ findVariable context "a" ] context
    ((ahR - aR) / h, context)
    
let builtinDerive (functionArg: Function) (context: Context) =
    let context = { context with message = Some (printFunction functionArg) }
    (0.0, context)
    
let rec simpsons (functionArg: Function) (context: Context) (amount: float) (a: float) (b: float) (n: int) (step: int) =
    let h = (b - a) / float n
    
    let yn, _ = evalFunction functionArg [ Node.Number(a + float step * h) ] context
    
    if step = 0 then
        simpsons functionArg context yn a b n (step + 1)
    else if step = n then
        (h / 3.0) * (amount + yn)
    else if step % 2 = 1 then
        simpsons functionArg context (amount + 4.0 * yn) a b n (step + 1)
    else
        simpsons functionArg context (amount + 2.0 * yn) a b n (step + 1)
        
let builtinIntegrate (functionArg: Function) (context: Context) =
    let a, _ = walk (findVariable context "a") context
    let b, _ = walk (findVariable context "b") context
    ((simpsons functionArg context 0.0 a b 1000 0), context)
    
let rec sum (functionArg: Function) (context: Context) (amount: float) (a: float) (b: float) =
    let y, _ = evalFunction functionArg [ Node.Number(a) ] context
    
    if a = b then
        amount + y
    else
        sum functionArg context (amount + y) (a + 1.0) b

let builtinSum (functionArg: Function) (context: Context) = 
    let a, _ = walk (findVariable context "a") context
    let b, _ = walk (findVariable context "b") context
    (sum functionArg context 0.0 a b, context)
        
let rec product (functionArg: Function) (context: Context) (amount: float) (a: float) (b: float) =
    let y, _ = evalFunction functionArg [ Node.Number(a) ] context
    
    if a = b then
        amount * y
    else
        product functionArg context (amount * y) (a + 1.0) b
    
let builtinProduct (functionArg: Function) (context: Context) =
    let a, _ = walk (findVariable context "a") context
    let b, _ = walk (findVariable context "b") context
    (product functionArg context 1.0 a b, context)
    
// Functions that run over possible functional arguments
// Needed because on builtinnumerical and userdefined the compiler does immediate evaluation
let builtinFunctional =
    Map.empty
        .Add("derive", Function.BuiltInFunctional({
            FunctionAssignmentInfo.name = "derive";
            FunctionAssignmentInfo.arguments = ["fx"; "a";];
        }, builtinDerive))
        .Add("integrate", Function.BuiltInFunctional({
            FunctionAssignmentInfo.name = "integrate"
            FunctionAssignmentInfo.arguments = ["fx"; "a"; "b";];
        }, builtinIntegrate))
        .Add("sum", Function.BuiltInFunctional({
            FunctionAssignmentInfo.name = "sum"
            FunctionAssignmentInfo.arguments = ["fx"; "a"; "b";];
        }, builtinSum))
        .Add("product", Function.BuiltInFunctional({
            FunctionAssignmentInfo.name = "product"
            FunctionAssignmentInfo.arguments = ["fx"; "a"; "b";];
        }, builtinProduct))
        
