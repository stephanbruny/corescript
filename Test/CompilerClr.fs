namespace CoreScript.Test

open NUnit.Framework
open FsUnitTyped
open Microsoft.FSharp.Reflection

open CoreScript
open CoreScript.Lexer
open CoreScript.Parser
open CoreScript.Tokens
open CoreScript.Expressions
open CoreScript.Interpreter
open CoreScript.Interpreter.Runtime
open CoreScript.Interpreter.Environment
open CoreScript.Compiler
open System.Numerics

open System
open System.Dynamic
open System.Linq.Expressions
open System.Collections.Generic

module CompilerClrTests =

    let scanAndParse text = Lexer.execute [] text 1 |> Parser.execute []
    let mutable scope = new CoreScope()

    [<Test>]
    let ``should convert to clr expressions`` () =
        let text = "let x = 2 + 2; 5 + x * x"
        let ast = scanAndParse text
        let tok = ast |> Block
        let expr = CompilerClr.compile (ref scope) tok
        printfn "SCOPE: %A" scope
        let result = Expression.Lambda<Func<obj>>(expr).Compile();
        result.Invoke() |> shouldEqual (21 :> obj)
    
    [<Test>]
    let ``should compile a function`` () =
        let text = "function foo (x) { x * x }; foo(8) + 3"
        let ast = scanAndParse text
        let tok = ast |> Block
        let expr = CompilerClr.compile (ref scope) tok
        printfn "SCOPE: %A" scope
        let result = Expression.Lambda<Func<obj>>(expr).Compile();
        result.Invoke() |> shouldEqual (67 :> obj)