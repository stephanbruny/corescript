namespace CoreScript.Interpreter.RuntimeModules

open CoreScript
open CoreScript.Interpreter
open CoreScript.Interpreter.Environment

open System
open System.IO
open System.Text
open System.Text.RegularExpressions

module String =

    let checkArgLength minLen (values: Environment.Value list) =
        if minLen > values.Length then
            failwith (sprintf "Insufficient arguments (expected: %i, given: %i)" minLen values.Length)

    let toUpperCase (call : NativeFunction<Value>) =
        checkArgLength 1 call.arguments
        match call.arguments.Head with
        | StringVal str -> str.ToUpper() |> StringVal
        | _ -> failwith "string expected"

    let strLength (call : NativeFunction<Value>) =
        checkArgLength 1 call.arguments
        match call.arguments.Head with
        | StringVal str -> str.Length |> IntVal
        | _ -> failwith "string expected"

    let toLowerCase (call : NativeFunction<Value>) =
        checkArgLength 1 call.arguments
        match call.arguments.Head with
        | StringVal str -> str.ToLower() |> StringVal
        | _ -> failwith "string expected"

    let toChars (call : NativeFunction<Value>) =
        checkArgLength 1 call.arguments
        match call.arguments.Head with
        | StringVal str -> 
            let chars = str.ToCharArray ()
            chars |> Array.mapi(fun i v -> (IntKey i, StringVal (v.ToString()) )) |> Map.ofArray |> TableVal
        | _ -> failwith "string expected"

    let substr (call: NativeFunction<Value>) = 
        checkArgLength 2 call.arguments
        let str = Environment.getStringArg call.arguments.Head
        let beginStr = getIntArg call.arguments.Tail.Head
        let endStr = 
            if call.arguments.Length > 2 then 
                getIntArg call.arguments.Tail.Tail.Head
            else str.Length - beginStr
        str.Substring(beginStr, endStr) |> StringVal

    let charToRegexOption item =
        match item with
        | 'i' -> RegexOptions.IgnoreCase
        | 'm' -> RegexOptions.Multiline
        | _ -> failwith "invalid option"

    let parseRegexOptions (str : string) =
        str.ToCharArray()
        |> Array.map charToRegexOption 
        |> Array.fold (|||) RegexOptions.None

    let testString (call : NativeFunction<Value>) =
        checkArgLength 2 call.arguments
        let str = getStringArg call.arguments.Head
        let regexstr = getStringArg call.arguments.Tail.Head
        let regex = 
            if call.arguments.Length > 2 then
                let regexOpts = getStringArg call.arguments.Tail.Tail.Head
                Regex(regexstr, parseRegexOptions regexOpts)
            else
                Regex(regexstr)
        regex.IsMatch(str, 0) |> BoolVal

    let replace (call : NativeFunction<Value>) =
        checkArgLength 3 call.arguments
        let str = getStringArg call.arguments.Head
        let search = getStringArg call.arguments.Tail.Head
        let repl = getStringArg call.arguments.Tail.Tail.Head
        str.Replace(search, repl) |> StringVal
    
    // let matchString (call : NativeFunction<Value>) =
    //     checkArgLength 2 call.arguments
    //     let str = getStringArg call.arguments.Head
    //     let regexstr = getStringArg call.arguments.Tail.Head
    //     let regex = 
    //         if call.arguments.Length > 2 then
    //             let regexOpts = getStringArg call.arguments.Tail.Tail.Head
    //             Regex(regexstr, parseRegexOptions regexOpts)
    //         else
    //             Regex(regexstr)
    //     let matches = regex.Matches str
    //     let results =
    //         matches
    //         |> Seq.cast<MatchCollection>
    //         |> Seq.mapi(fun i d -> (i, d.Item i))
    //         |> Seq.map(fun (i, m) -> m.Groups)

    let stringTable = 
        [
            (Key "length", NativeFunVal strLength);
            (Key "toUpperCase", NativeFunVal toUpperCase);
            (Key "toLowerCase", NativeFunVal toLowerCase);
            (Key "toChars", NativeFunVal toChars);
            (Key "replace", NativeFunVal replace);
            (Key "test", NativeFunVal testString);
            (Key "substr", NativeFunVal substr);
        ] |> Map.ofList |> TableVal