// Learn more about F# at http://fsharp.org

open System
open System.IO
open CoreScript
open CoreScript.Interpreter
open CoreScript.Tokens
open CoreScript.Compiler

let readFile filePath = File.ReadAllText filePath

[<EntryPoint>]
let main argv =
    match argv with
    | [||] -> 
        let rec repl (scope : Interpreter.Environment.Scope<Interpreter.Environment.Value>) =
            Console.Write("> ");
            try
                let input = Console.ReadLine ()
                let ast = CoreScript.Lexer.execute [] input 0 |> Parser.execute []
                let (next, _) = Interpreter.eval scope (ast |> Tokens.Block)
                repl next
            with 
            | ex -> 
                eprintfn "%s" ex.Message
                repl scope
        repl Interpreter.Runtime.defaultScope
        0
    | _ -> 
        let path = argv |> Array.last
        let source = readFile path
        let ast = CoreScript.Lexer.execute [] source 1 |> Parser.execute []
        let globalScope = Interpreter.Runtime.defaultScope
        let interpret (scope : Interpreter.Environment.Scope<Interpreter.Environment.Value>) (tokens : Token list) =
            Interpreter.eval scope (tokens |> Tokens.Block)
        // printfn "AST: %A" ast
        try
            let (_, returnValue) = interpret globalScope ast
            match returnValue with
            | Interpreter.Environment.IntVal i -> i
            | _ -> 0
        with
        | ex -> 
            eprintfn "Runtime error: %A" ex
            -1
