namespace saxon_tests

open NUnit.Framework
open saxon.Wrapper

[<TestFixture>]
type TestClass () =
    let createWrapperAndRun (input: string) =
        let saxon = SaxonWrapper()
        saxon.runInput input
    
    let withinError (threshold: float) (a: float) (b: float) =
        abs(a - b) <= threshold
    
    let acceptableError = withinError 0.0001
    
    [<Test>]
    member this.Arithmetic1() =
        let result = createWrapperAndRun "9249 - 2297 + 1152 / 12 + 4819 + 5040 - 4812 + 570 * 27"
        Assert.AreEqual(acceptableError (float result) 27485, true)
     
    [<Test>]
    member this.Arithmetic2() =
        let result = createWrapperAndRun "(430.123 - 299.12) ^ 3.14159 + 27 * -2"
        Assert.AreEqual(acceptableError (float result) 4483574.0201296667, true)
        
    [<Test>]
    member this.DivisionTest() =
        let result = createWrapperAndRun "2 / 4 * 2"
        Assert.AreEqual(acceptableError (float result) 1, true)
        
    [<Test>]
    member this.Constants() =
        let result = createWrapperAndRun "e * pi * tau"
        Assert.AreEqual(acceptableError (float result) 53.65673260, true)
        
    [<Test>]
    member this.TrigFunctions() =
        let result = createWrapperAndRun "sin(pi/2) + cos(pi/4) + tan(pi/8)"
        Assert.AreEqual(acceptableError (float result) 2.121320344, true)
        
    [<Test>]
    member this.TrigFunctions2() =
        let result = createWrapperAndRun "pi * csc(1) + pi * sec(1) + pi * cot(1)"
        Assert.AreEqual(acceptableError (float result) 11.56515584, true)
    
    [<Test>]
    member this.TrigFunctions3() =
        let result = createWrapperAndRun "arccos(0) + arcsin(0) + arctan(1)"
        Assert.AreEqual(acceptableError (float result) 2.356194490, true)
        
    [<Test>]
    member this.Logarithms() =
        let result = createWrapperAndRun "ln(e^4) - log(2, 8)"
        Assert.AreEqual(acceptableError (float result) 1.0, true)
    
    [<Test>]
    member this.NumberTheory() =
        let result = createWrapperAndRun "lcm(70, 25) / gcd(9, 27) + mod(12, 7) + abs(-452)"
        Assert.AreEqual(acceptableError (float result) 495.888888889, true)
        
    [<Test>]
    member this.Roots() =
        let result = createWrapperAndRun "sqrt(576) + cbrt(1000) + root(4, 4096)"
        Assert.AreEqual(acceptableError (float result) 42.0, true)
        
    [<Test>]
    member this.UserVariables() =
        let saxon: SaxonWrapper = SaxonWrapper()
        saxon.runInput "let x = 3 * 12" |> ignore
        saxon.runInput "let y = 4"  |> ignore
        let result = saxon.runInput "x + y"
        Assert.AreEqual(acceptableError (float result) 40.0, true)
        
    [<Test>]
    member this.UserFunctions() =
        let saxon = SaxonWrapper()
        saxon.runInput "let f(x) = x^2 + 2 * x - 3" |> ignore
        let result = saxon.runInput "f(3)"
        Assert.AreEqual(acceptableError (float result) 12.0, true)
        
    [<Test>]
    member this.Integrate() =
        let saxon = SaxonWrapper()
        saxon.runInput "let h(x) = x" |> ignore
        let result = saxon.runInput "integrate(h, 0, 2)"
        Assert.AreEqual(acceptableError (float result) 2.0, true)
        
    [<Test>]
    member this.ProductAndSum() =
        let saxon = SaxonWrapper()
        saxon.runInput "let identity(x) = x" |> ignore
        saxon.runInput "let factorial(x) = product(identity, 1, x)" |> ignore
        saxon.runInput "let sumtorial(x) = sum(identity, 1, x)" |> ignore
        let result = saxon.runInput "factorial(5) + sumtorial(10)"
        Assert.AreEqual(acceptableError (float result) 175.0, true)