namespace CoreScript.Test

open NUnit.Framework
open FsUnitTyped
open Microsoft.FSharp.Reflection

open CoreScript
open CoreScript.Lexer
open CoreScript.Parser
open CoreScript.Expressions
open CoreScript.Interpreter
open CoreScript.Interpreter.Runtime
open CoreScript.Interpreter.Environment
open System.Numerics

module ExpressionsTest =

    let scanAndParse text = Lexer.execute [] text 1 |> Parser.execute []

    let testScope = Interpreter.Runtime.defaultScope

    let assertStringVal str value =
        match value with
        | StringVal s -> s |> shouldEqual str
        | _ -> failwith (sprintf "Expected %A, but was: %A" (StringVal str) value)

    let rec assertValue expected value =
        let eq a b = b |> shouldEqual a
        match (expected, value) with
        | (StringVal a, StringVal b) -> eq a b
        | (IntVal a, IntVal b) -> eq a b
        | (BoolVal a, BoolVal b) -> eq a b
        | (DoubleVal a, DoubleVal b) -> eq a b
        | (TableVal a, TableVal b) -> 
            a |> Map.iter(fun k v ->
                let bVal = b |> Map.tryFind k
                match bVal with
                | Some vb -> assertValue v vb
                | None _ -> failwith (sprintf "Expected key %A to be found, but was empty" k)
            )
        | (ErrorVal a, ErrorVal b) -> eq a b
        | (ReturnVal a, ReturnVal b) -> assertValue a b
        | (NoneVal, NoneVal) -> ()
        | (UnitVal, UnitVal) -> ()
        | _ -> failwith (sprintf "Expected %A, but was: %A" expected value)
        

    [<Test>]
    let ``should parse an expression with precedence matching`` () =
        let text = "3 + 5 * 3 - 2"

        let expectedResult = IntVal (int32 16)

        let ast = scanAndParse text
        printfn "AST 1: %A" ast
        let (_, result) = Interpreter.eval testScope ast.Head
        assertValue expectedResult result

        let text2 = "2 * 5 + 2 / 2"
        let ast2 = scanAndParse text2
        let (_, result2) = Interpreter.eval testScope ast2.Head
        printfn "AST: %A" ast2
        assertValue (DoubleVal 11.0) result2

    [<Test>]
    let ``should parse an expression with groups/braces`` () =
        let text = "(3 + 5) * (3 - 2)"

        let expectedResult = IntVal (int32 8)

        let ast = scanAndParse text
        let (_, result) = Interpreter.eval testScope ast.Head
        assertValue expectedResult result

    [<Test>]
    let ``should parse an expression including a variable with precedence matching`` () =
        let text = 
            """
            let x = 4;
            3 + 5 * x - 2
            """

        let expectedResult = IntVal (int32 21)

        let ast = scanAndParse text
        printfn "Ast: %A" ast
        let (_, result) = Interpreter.eval testScope (ast |> Tokens.Block)
        assertValue expectedResult result

    [<Test>]
    let ``should parse an expression including a function call`` () =
        let text = 
            """
            function getX() 1 * 4;
            3 + 5 * getX() - 2;
            """

        let expectedResult = IntVal (int32 21)

        let ast = scanAndParse text
        printfn "AST: %A" ast
        let (_, result) = Interpreter.eval testScope (ast |> Tokens.Block)
        assertValue expectedResult result
    
    [<Test>]
    let ``should parse an expression including a only variables`` () =
        let text = 
            """
            let a = 1;
            let b = 2;
            let c = 3;
            let d = 4;
            a + b * c / d - a
            """

        let expectedResult = DoubleVal 1.5

        let ast = scanAndParse text
        printfn "AST: %A" ast
        let (_, result) = Interpreter.eval testScope (ast |> Tokens.Block)
        assertValue expectedResult result

    [<Test>]
    let ``evaluate let-binding and put into scope`` () =
        let text = "let foo = 'bar'"
        let ast = scanAndParse text
        let (scope, result) = ast.Head |> Interpreter.eval testScope
        assertValue UnitVal result
        let item = scope.local.TryFind "foo"
        item |> Option.isSome |> shouldEqual true
        assertValue (StringVal "bar") (item |> Option.get)

    [<Test>]
    let ``should bind an expression`` () =
        let text = "let foo = 2 * 5 + 2 / 2;"
        let ast = scanAndParse text
        let (scope, result) = ast.Head |> Interpreter.eval testScope
        assertValue UnitVal result
        let item = scope.local.TryFind "foo"
        item |> Option.isSome |> shouldEqual true
        printfn "AST %A" ast
        assertValue (DoubleVal 11.0) (item |> Option.get)

    [<Test>]
    let ``should call a function from default runtime`` () =
        let text = "typeof('foo')"
        let ast = scanAndParse text
        let (_, result) = ast.Head |> Interpreter.eval Runtime.defaultScope
        assertValue (StringVal "string") result
    
    [<Test>]
    let ``should call with table argument`` () =
        let text = "typeof({ foo: 'bar' })"
        let ast = scanAndParse text
        let (_, result) = ast.Head |> Interpreter.eval Runtime.defaultScope
        assertValue (StringVal "table") result

    [<Test>]
    let ``should parse if-statement`` () =
        let text = "if (3 > 4) { 'correct' } else { 'wrong' }"
        let ast = scanAndParse text
        let (_, result) = ast.Head |> Interpreter.eval testScope
        assertValue (StringVal "wrong") result

    [<Test>]
    let ``should bind lambda`` () =
        let text = 
            """
            let lambda = fun (x, y) x * y;
            lambda(2, 2)
            """
        let ast = scanAndParse text
        let (scope, result) = (ast |> Tokens.Block) |> Interpreter.eval testScope
        printfn "Ast: %A { %A }" ast scope
        assertValue (IntVal 4) result

    [<Test>]
    let ``should call a function with expression argument`` () =
        let text = 
            """
            let lambda = fun (x, y) x * y;
            lambda(1 + 2, 2) + 3;
            """
        let ast = scanAndParse text
        let (scope, result) = (ast |> Tokens.Block) |> Interpreter.eval testScope
        printfn "Ast: %A" ast
        assertValue (IntVal 9) result

    [<Test>]
    let ``should re-assign a mutable`` () =
        let text = 
            """
            var variable = 3;
            variable <- variable + 2;
            variable
            """
        let ast = scanAndParse text
        let (scope, result) = (ast |> Tokens.Block) |> Interpreter.eval testScope
        printfn "Ast: %A" ast
        assertValue (IntVal 5) result

    [<Test>]
    let ``should bind a table`` () =
        let text = 
            """
            let table = { foo: 'foo', bar: '42', 'some-key': 1.234 };
            table
            """
        let ast = scanAndParse text
        let (scope, result) = (ast |> Tokens.Block) |> Interpreter.eval testScope
        printfn "Ast: %A" ast
        let tableContents = 
            [
                (JTableKey.Key "foo", StringVal "foo");
                (JTableKey.Key "bar", StringVal "42");
                (JTableKey.Key "some-key", DoubleVal 1.234);
            ] |> Map.ofList
        assertValue (TableVal tableContents) result

    [<Test>]
    let ``should catch an exception`` () =
        let text = 
            """
            try {
                throw "Exception raised";
            } catch ex {
                return ex;
            }
            """

        let ast = scanAndParse text
        printfn "AST: %A" ast
        let (_, result) = Interpreter.eval testScope ast.Head
        assertValue (ReturnVal (ErrorVal "Exception raised")) result

    [<Test>]
    let ``should get nested value from a table`` () =
        let text = 
            """
            let table = { foo: { bar: 42 } };
            table.foo.bar
            """
        let ast = scanAndParse text
        let (scope, result) = (ast |> Tokens.Block) |> Interpreter.eval testScope
        printfn "Ast: %A" ast
        assertValue (IntVal 42) result

    [<Test>]
    let ``should use a nested value in expression`` () =
        let text = 
            """
            let table = { foo: { bar: 42 } };
            2 * table.foo.bar
            """
        let ast = scanAndParse text
        let (scope, result) = (ast |> Tokens.Block) |> Interpreter.eval testScope
        printfn "Ast: %A" ast
        assertValue (IntVal 84) result

    [<Test>]
    let ``should define mutable table property`` () =
        let text = 
            """
            let table = { foo: { mutable bar: 0 } };
            table.foo.bar <- 42;
            2 * table.foo.bar
            """
        let ast = scanAndParse text
        printfn "Ast: %A" ast
        let (scope, result) = (ast |> Tokens.Block) |> Interpreter.eval testScope
        printfn "Scope: %A" scope
        assertValue (IntVal 84) result

    [<Test>]
    let ``should extend a table`` () =
        let text = 
            """
            let parent = { foo: 'bar' };
            let child = extend parent { bar: '42' };
            child.foo + child.bar
            """
        let ast = scanAndParse text
        printfn "Ast: %A" ast
        let (_, result) = (ast |> Tokens.Block) |> Interpreter.eval testScope
        assertValue (StringVal ("bar42")) result

    [<Test>]
    let ``should parse prefix (!) operator`` () =
        let text = 
            """
            !true;
            """
        let ast = scanAndParse text
        printfn "Ast: %A" ast
        let (_, result) = (ast |> Tokens.Block) |> Interpreter.eval testScope
        assertValue (BoolVal false) result

    [<Test>]
    let ``should parse prefix (-) operator`` () =
        let text = 
            """
            -(3 + 5)
            """
        let ast = scanAndParse text
        printfn "Ast: %A" ast
        let (_, result) = (ast |> Tokens.Block) |> Interpreter.eval testScope
        assertValue (IntVal -8) result