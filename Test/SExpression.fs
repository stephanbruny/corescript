namespace CoreScript.Test

open NUnit.Framework
open FsUnitTyped

open CoreScript
open CoreScript.Lexer
open CoreScript.Parser
open CoreScript.Expressions
open CoreScript.Backend
open CoreScript.Interpreter.Environment

open System.Collections
open CoreScript.Interpreter

module SExpressionTest =

    let scanAndParse text = Lexer.execute [] text 1 |> Parser.execute []

    [<Test>]
    let ``should transpile an expression with precedence matching`` () =
        let text = "3 + 5 * 3 - 2"
        let ast = scanAndParse text
        printfn "AST: %A" ast
        let sexpressions = SExpression.transpile [] ast
        let result = SExpression.serialize sexpressions
        result |> shouldEqual "(+ 3 (- (* 5 3) 2))"

    [<Test>]
    let ``should transpile an let binding`` () =
        let text = "let foo = 3 + 5 * 3 - 2;"
        let ast = scanAndParse text
        printfn "AST: %A" ast
        let sexpressions = SExpression.transpile [] ast
        let result = SExpression.serialize sexpressions
        result |> shouldEqual "(define foo (+ 3 (- (* 5 3) 2)))"

    [<Test>]
    let ``should transpile function call`` () =
        let text = "foo.fn(3, 4, 5 + 6)"
        let ast = scanAndParse text
        printfn "AST: %A" ast
        let sexpressions = SExpression.transpile [] ast
        let result = SExpression.serialize sexpressions
        result |> shouldEqual "(foo.fn 3 4 (+ 5 6))"
    
    [<Test>]
    let ``should transpile function definition`` () =
        let text = "function foo(x, y) { x + y; }"
        let ast = scanAndParse text
        printfn "AST: %A" ast
        let sexpressions = SExpression.transpile [] ast
        let result = SExpression.serialize sexpressions
        result |> shouldEqual "(defun foo (x y) (+ x y))"

    [<Test>]
    let ``should transpile lambda definition`` () =
        let text = "let foo = fun (x, y) { x + y; }"
        let ast = scanAndParse text
        printfn "AST: %A" ast
        let sexpressions = SExpression.transpile [] ast
        let result = SExpression.serialize sexpressions
        result |> shouldEqual "(define foo (lambda (x y) (+ x y)))"

    [<Test>]
    let ``should transpile function definition and call to var binding`` () =
        let text = "function foo(x, y) { x + y; }; var x = foo(3, 4);"
        let ast = scanAndParse text
        printfn "AST: %A" ast
        let sexpressions = SExpression.transpile [] ast
        let result = SExpression.serialize sexpressions
        result |> shouldEqual "(defun foo (x y) (+ x y)) (defvar x (foo 3 4))"

    [<Test>]
    let ``should transpile calling a lambda`` () =
        let text = "(fun(x) x * x)(5);"
        let ast = scanAndParse text
        printfn "AST: %A" ast
        let sexpressions = SExpression.transpile [] ast
        let result = SExpression.serialize sexpressions
        result |> shouldEqual "((lambda (x) (* x x)) 5)"

    [<Test>]
    let ``should transpile if-statement with else`` () =
        let text = "if (x > y) { x + 1; } else { x - 1 }"
        let ast = scanAndParse text
        printfn "AST: %A" ast
        let sexpressions = SExpression.transpile [] ast
        let result = SExpression.serialize sexpressions
        result |> shouldEqual "(if (> x y) (+ x 1) (- x 1))"

    [<Test>]
    let ``should transpile if-statement without else`` () =
        let text = "if (x > y) { x + 1; }"
        let ast = scanAndParse text
        printfn "AST: %A" ast
        let sexpressions = SExpression.transpile [] ast
        let result = SExpression.serialize sexpressions
        result |> shouldEqual "(if (> x y) (+ x 1))"

    [<Test>]
    let ``should transpile a table`` () =
        let text = "let foo = { foo: 'bar', 'foo-fn': fun () { 'foo' }, mutable bar: 42 }"
        let ast = scanAndParse text
        printfn "AST: %A" ast
        let sexpressions = SExpression.transpile [] ast
        let result = SExpression.serialize sexpressions
        result |> shouldEqual """(define foo (table (:foo "bar") (:foo-fn (lambda () "foo")) (:$bar 42)))"""

    [<Test>]
    let ``should transpile a while loop`` () =
        let text = "while (x > 0) { x <- x - 1; }"
        let ast = scanAndParse text
        printfn "AST: %A" ast
        let sexpressions = SExpression.transpile [] ast
        let result = SExpression.serialize sexpressions
        result |> shouldEqual """(while (> x 0) (setvar x (- x 1)))"""


    [<Test>]
    let ``should transpile match case`` () =
        let text = 
            """
            match (foo.bar) {
                case ({ "foo": 123 }) true;
                case ({ "foo": some }) false;
                default { throw "failed" };
            }
            """
        let ast = scanAndParse text
        printfn "AST: %A" ast
        let sexpressions = SExpression.transpile [] ast
        let result = SExpression.serialize sexpressions
        printfn "Result: %s" result
        result |> shouldEqual """(match foo.bar (match-case (table (:foo 123)) True) (match-case (table (:foo some)) False) (default-case (throw "failed")))"""
