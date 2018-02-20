namespace CoreScript.Interpreter

open System
open System.IO
open System.Text
open System.Reflection
open CoreScript
open CoreScript.Interpreter.Environment

module Runtime =

    let findInnerOrOuter scope name = joinMap scope.local scope.outer |> Map.tryFind name

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

    let rec nativePrint (call : Environment.NativeFunction<Environment.Value>) =
        let strings = call.arguments |> List.map (getString call)
        Console.WriteLine (strings |> String.concat "")
        Environment.NoneVal

    let rec nativeTypeof (call : Environment.NativeFunction<Environment.Value>) = 
        let value = call.arguments.Head
        match value with
        | Environment.Task _ -> Environment.StringVal "task"
        | Environment.AsyncVal _ -> Environment.StringVal "async"
        | Environment.StringVal _ -> Environment.StringVal "string"
        | Environment.IntVal _ -> Environment.StringVal "integer"
        | Environment.DoubleVal _ -> Environment.StringVal "double"
        | Environment.BoolVal _ -> Environment.StringVal "bool"
        | Environment.FunVal _ -> Environment.StringVal "function"
        | Environment.NativeFunVal _ -> Environment.StringVal "function"
        | Environment.CollectionVal _ -> Environment.StringVal "collection"
        | Environment.TableVal _ -> Environment.StringVal "table"
        | Environment.ErrorVal _ -> Environment.StringVal "error"
        | Environment.NoneVal _ -> Environment.StringVal "none"
        | Environment.UnitVal _ -> Environment.StringVal "unit"
        | Environment.Expression _ -> Environment.StringVal "expression"
        | Environment.ReturnVal v -> nativeTypeof({ arguments = [v]; scope = call.scope; eval = call.eval})
        | Environment.AgentVal (v, _) -> nativeTypeof({ arguments = [v]; scope = call.scope; eval = call.eval})
        | Environment.ReceiverVal v -> nativeTypeof({ arguments = [v]; scope = call.scope; eval = call.eval})
        | Environment.Mutable v -> nativeTypeof({ arguments = [v.Value]; scope = call.scope; eval = call.eval})
        | Environment.RefVal name ->
            let ref = (name |> Environment.nameToString) |> findInnerOrOuter call.scope
            match ref with
            | Some v -> nativeTypeof ({ arguments = [v]; scope = call.scope; eval = call.eval})
            | Option.None -> Environment.NoneVal

    let rec nativeWaitAsync (call : Environment.NativeFunction<Environment.Value>) = 
        let value = call.arguments.Head
        match value with
        | Environment.IntVal i ->
            let task = async { 
                Threading.Thread.Sleep i 
                return value
            }
            Environment.AsyncVal task
        | _ -> Environment.UnitVal

    let rec nativeWaitSync (call : Environment.NativeFunction<Environment.Value>) = 
        let value = call.arguments.Head
        match value with
        | Environment.IntVal i ->
            Threading.Thread.Sleep i 
            Environment.IntVal i
        | _ -> Environment.UnitVal

    let rec nativeAssert (call : Environment.NativeFunction<Environment.Value>) = 
        if call.arguments.Length < 1 then failwith "[assert] expected at least 1 argument"
        let value = call.arguments.Head
        let errorMessage =
            if (call.arguments.Length > 1) then
                match call.arguments.Tail.Head with
                | Environment.StringVal str -> str
                | _ -> failwith "[assert] expected 2nd argument to be string"
            else
                "assertion failed"
        match value with
        | Environment.BoolVal b -> if b then Environment.UnitVal else failwith errorMessage
        | Environment.NoneVal -> failwith errorMessage
        | _ -> Environment.UnitVal

    let parseInt (call : Environment.NativeFunction<Environment.Value>) =
        if call.arguments.Length < 1 then failwith "[parseInt] expected 1 argument"
        match call.arguments.Head with
        | StringVal str -> 
            match Int32.TryParse str with
            | (true, i) -> IntVal i
            | _ -> NoneVal
        | _ -> failwith "string expected"

    let parseDouble (call : Environment.NativeFunction<Environment.Value>) =
        if call.arguments.Length < 1 then failwith "[parseDouble] expected 1 argument"
        match call.arguments.Head with
        | StringVal str -> 
            try
                Double.Parse(str, Globalization.CultureInfo.InvariantCulture) |> DoubleVal
            with
            | _ -> NoneVal
        | _ -> failwith "string expected"

    let toString (call : Environment.NativeFunction<Environment.Value>) =
        if call.arguments.Length < 1 then failwith "[toString] expected 1 argument"
        getString call call.arguments.Head |> StringVal
        

    let getRuntimeModule name =
        match name with
        | "filesystem" -> RuntimeModules.Filesystem.apiTable
        | "http" -> RuntimeModules.Http.httpTable
        | _ -> Environment.NoneVal

    let processEnv =
        Environment.GetEnvironmentVariables()
        |> Seq.cast<System.Collections.DictionaryEntry>
        |> Seq.map (fun d -> d.Key :?> string, d.Value :?> string)
        |> Array.ofSeq
        |> Array.Parallel.map(fun (k, v) -> (Key k, StringVal v))
        |> Map.ofArray
        |> TableVal

    let processDir = Environment.CurrentDirectory |> StringVal
    let processArgv = 
        Environment.GetCommandLineArgs ()
        |> List.ofArray
        |> List.map StringVal
        |> List.tail
        |> List.mapi(fun i v -> (IntKey i, v))
        |> Map.ofList
        |> TableVal

    let processTable =
        [
            (Key "env", processEnv);
            (Key "dir", processDir);
            (Key "argv", processArgv);
        ] |> Map.ofList |> TableVal

    let defaultRuntime = 
        [ 
            ("print", Environment.NativeFunVal nativePrint); 
            ("typeof", Environment.NativeFunVal nativeTypeof);
            ("parseInt", Environment.NativeFunVal parseInt); 
            ("parseDouble", Environment.NativeFunVal parseDouble); 
            ("toString", Environment.NativeFunVal toString); 
            ("waitAsync", Environment.NativeFunVal nativeWaitAsync);
            ("assert", Environment.NativeFunVal nativeAssert);
            ("wait", Environment.NativeFunVal nativeWaitSync);
            ("array", Environment.NativeFunVal RuntimeModules.Array.createArray);
            ("table", Environment.TableVal RuntimeModules.Tables.tablesMethods);
            ("Math", RuntimeModules.Math.mathTable);
            ("string", RuntimeModules.String.stringTable);
            ("process", processTable);
        ] |> Map.ofList

    let defaultScope = { outer = defaultRuntime; local = Map.empty }

    let importDll (filePath : string) =
        try
            let assembly = Assembly.LoadFile filePath
            let types = assembly.GetExportedTypes()
            let exportType = types |> Array.find (fun tp ->
                try
                    tp.GetMember("coreScriptExport") |> ignore
                    true
                with
                | _ -> false
            )
            exportType.InvokeMember("coreScriptExport", BindingFlags.GetProperty, null, null, [||], Globalization.CultureInfo.InvariantCulture) :?> Value
        with
        | ex -> failwith (sprintf "Could not import DLL: %s" ex.Message)
        
    let importModule (file : string) (eval : Scope<'T> -> CoreScript.Tokens.Token -> Scope<'T> * Value) =
        let maybeRuntimeModule = getRuntimeModule file
        match maybeRuntimeModule with
        | NoneVal ->
            let cmdLine = Environment.GetCommandLineArgs ()
            let cwd = cmdLine.[1] |> IO.Path.GetDirectoryName
            let filePath = IO.Path.Combine [|cwd; file|]
            if IO.File.Exists filePath then
                try 
                    if Path.GetExtension filePath = ".dll" then
                        (defaultScope, importDll (filePath |> Path.GetFullPath))
                    else
                        let fileSource = File.ReadAllText filePath
                        let scan = Lexer.execute [] fileSource 1
                        let ast = scan |> Parser.execute []
                        eval defaultScope (ast |> Tokens.Block)
                with
                | ex -> failwith (sprintf "%s (in: %s)" ex.Message filePath)
            else
                failwith (sprintf "Cannot find module '%s'" filePath)
        | _ -> (defaultScope, maybeRuntimeModule)
