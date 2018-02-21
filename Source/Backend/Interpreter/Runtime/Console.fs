namespace CoreScript.Interpreter.RuntimeModules

open System
open System.Text

open CoreScript
open CoreScript.Interpreter
open CoreScript.Interpreter.Environment

module Console =
    let findInnerOrOuter (scope : Scope<Value>) name = 
        Map.fold (fun acc key value -> Map.add key value acc) scope.outer scope.local |> Map.tryFind name
    let unescapeString (str : string) =
        let builder = StringBuilder(str)
        builder
            .Replace("\\n", "\n") 
            .Replace("\\t", "\t")
            .Replace("\\r", "\r")
            .ToString()
    let rec getString (call : NativeFunction<Value>) v =
        let rec tableToString (table : Map<Environment.JTableKey, Value>) depth =
            let prefix = [0..depth] |> List.map(fun _ -> "  ") |> String.concat ""
            let endPrefix = if depth < 1 then "" else prefix.Substring(2)
            let strings = 
                table |> Map.map(fun key value ->
                    let keyString = 
                        match key with
                        | IntKey i -> i.ToString()
                        | Key s -> s
                    let valString = 
                        match value with
                        | TableVal tab -> tableToString tab (depth + 1)
                        | StringVal str -> sprintf "%A" str
                        | _ -> getString call value
                    [ prefix + keyString; valString] |> (String.concat ": ")
                ) 
                |> Map.toList |> List.map(fun (_, v) -> v)
            let propSep = ", \n"
            sprintf "{\n%s\n%s}" (strings |> String.concat propSep ) endPrefix
        match v with
        | Environment.StringVal s -> unescapeString s
        | Environment.IntVal i -> sprintf "%i" i
        | Environment.DoubleVal d -> sprintf "%f" d
        | Environment.BoolVal b -> b.ToString()
        | Environment.NoneVal -> "<none>"
        | Environment.UnitVal -> "<unit>"
        | Environment.FunVal (Option.None, b, c, d) -> sprintf "<lambda (%s)>" (b |> String.concat ", ")
        | Environment.FunVal (Some n, b, c, d) -> sprintf "<function %s (%s)>" n (b |> String.concat ", ")
        | Environment.RefVal n ->
            match (n |> Environment.nameToString) |> findInnerOrOuter call.scope with
            | Some value -> getString call value
            | None -> "<none>"
        | Environment.TableVal tableMap -> tableToString tableMap 0
        | _ -> sprintf "%A" v

    let consoleOut outFn (call : NativeFunction<Value>) =
        let strings = call.arguments |> List.map (getString call)
        outFn (strings |> String.concat " ")
        UnitVal

    let consoleLog = consoleOut Console.WriteLine
    let consoleError = consoleOut Console.Error.WriteLine
    let consoleReadLine (call : NativeFunction<Value>) = 
        let input = Console.ReadLine ()
        StringVal input

    let consoleColor (call : NativeFunction<Value>) =
        match call.arguments |> List.tryItem 0 with
        | Some (IntVal i) -> Console.ForegroundColor <- enum<ConsoleColor>(i)
        | _ -> Console.ForegroundColor <- ConsoleColor.Gray
        UnitVal

    let consoleTable = 
        [
            (Key "log", NativeFunVal consoleLog);
            (Key "error", NativeFunVal consoleError);
            (Key "readln", NativeFunVal consoleReadLine);
            (Key "color", NativeFunVal consoleColor);
        ] |> Map.ofList |> TableVal