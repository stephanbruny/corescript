// Learn more about F# at http://fsharp.org

open System
open System.IO
open CoreScript
open CoreScript.Interpreter
open CoreScript.Tokens
open CoreScript.Interpreter

let readFile filePath = File.ReadAllText filePath

[<EntryPoint>]
let main argv =
    match argv with
    | [||] -> 
        printfn "no input file"
        1
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
