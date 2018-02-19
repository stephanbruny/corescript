namespace CoreScript.Interpreter.RuntimeModules

open CoreScript
open CoreScript.Interpreter
open CoreScript.Interpreter.Environment

open System
open System.IO

module Filesystem =
    let exists path = File.Exists path

    let checkArgLength minLen (values: Environment.Value list) =
        if minLen > values.Length then
            failwith (sprintf "Insufficient arguments (expected: %i, given: %i)" minLen values.Length)

    let readText (call : NativeFunction<Value>) =
        checkArgLength 1 call.arguments
        let value = call.arguments.Head
        match value with
        | StringVal path ->
            if exists path then
                let fileContent = File.ReadAllText path
                StringVal fileContent
            else
                failwith (sprintf "[Filesystem.readText] File Not Found: %s" path)
        | _ -> failwith "[Filesystem.readText] invalid path given"

    let readBinary (call : NativeFunction<Value>) =
        checkArgLength 1 call.arguments
        let value = call.arguments.Head
        match value with
        | StringVal path ->
            if exists path then
                let fileContent = File.ReadAllBytes path
                let byteTableData = fileContent |> Array.Parallel.mapi(fun i item -> (JTableKey.IntKey i, IntVal (int32 item) ) ) |> Map.ofArray
                TableVal byteTableData
            else
                failwith (sprintf "[Filesystem.readBinary] File Not Found: %s" path)
        | _ -> failwith "[Filesystem.readBinary] invalid path given"

    let joinPath (call : NativeFunction<Value>) =
        let pathSeperator = System.IO.Path.PathSeparator.ToString()
        let rec join (current : string) (valueList : Environment.Value list) =
            match valueList with
            | [] -> current
            | head::tail ->
                match head with
                | StringVal str -> join ([current; str] |> String.concat pathSeperator) tail
                | _ -> failwith "[Filesystem.joinPath] path must be string"
        let result = join "" call.arguments
        StringVal result

    let writeText (call : NativeFunction<Value>) =
        checkArgLength 2 call.arguments
        let value = call.arguments.Head
        let dataValue = call.arguments.Tail.Head
        match value with
        | StringVal path ->
            if not (exists path) then
                failwith (sprintf "[Filesystem.writeText] File Not Found: %s" path)
            match dataValue with
            | StringVal text -> File.WriteAllText(path, text, Text.Encoding.UTF8)
            | _ -> failwith "[Filesystem.writeText] string expected"
            UnitVal
        | _ -> failwith "[Filesystem.writeText] invalid path given"
    
    let writeBinary (call : NativeFunction<Value>) =
        checkArgLength 2 call.arguments
        let value = call.arguments.Head
        let dataValue = call.arguments.Tail.Head
        match value with
        | StringVal path ->
            if not (exists path) then
                failwith (sprintf "[Filesystem.writeText] File Not Found: %s" path)
            match dataValue with
            | TableVal contents -> 
                let byteArray = 
                    contents 
                    |> Map.toArray
                    |> Array.filter(fun (k, _ ) -> match k with | IntKey _ -> true | _ -> false )
                    |> Array.sortWith(fun (ak, _) (bk, _) -> 
                        if ak < bk then -1 else if ak > bk then 1 else 0
                    )
                    |> Array.Parallel.map(fun (_, v) ->
                        match v with
                        | IntVal i when i >= 0 && i <= 255 -> (byte i)
                        | _ -> failwith "Invalid buffer value (must be integer between 0 - 255)"
                    )
                File.WriteAllBytes(path, byteArray)
            | _ -> failwith "[Filesystem.writeText] byte table expected"
            UnitVal
        | _ -> failwith "[Filesystem.writeText] invalid path given"

    let apiTable = 
        [
            (Key "joinPath", NativeFunVal joinPath);
            (Key "readText", NativeFunVal readText);
            (Key "readBinary", NativeFunVal readBinary);
            (Key "writeText", NativeFunVal writeText);
            (Key "writeBinary", NativeFunVal writeBinary);
        ] |> Map.ofList |> TableVal