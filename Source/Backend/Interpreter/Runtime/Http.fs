namespace CoreScript.Interpreter.RuntimeModules

open CoreScript.Interpreter
open CoreScript.Interpreter.Environment

open System
open System.Net
open System.Text

module Http =

    let checkArgLength minLen (values: Environment.Value list) =
        if minLen > values.Length then
            failwith (sprintf "Insufficient arguments (expected: %i, given: %i)" minLen values.Length)

    let getListener host (handler:(HttpListenerRequest->HttpListenerResponse->Async<unit>)) =
        let hl = new HttpListener()
        hl.Prefixes.Add host
        hl.Start()
        let task = Async.FromBeginEnd(hl.BeginGetContext, hl.EndGetContext)
        async {
            while true do
                let! context = task
                Async.Start(handler context.Request context.Response)
        } |> Async.Start
     
    let tableOfNameValueCollection (collection : System.Collections.Specialized.NameValueCollection) =
        collection.AllKeys
        |> Array.map(fun k -> (Key k, StringVal (collection.Get k) ))
        |> Map.ofArray
        |> TableVal

    let requestStreamToTable (req : HttpListenerRequest) =
        let stream = req.InputStream
        let streamRead (call : NativeFunction<Value>) =
            if req.HasEntityBody then
                checkArgLength 2 call.arguments
                let readBegin = match call.arguments.Head with | IntVal i -> i | _ -> failwith "integer expected"
                let readEnd = match call.arguments.Tail.Head with | IntVal i -> i | _ -> failwith "integer expected"
                let mutable buf = [||]
                stream.Read(buf, readBegin, readEnd) |> ignore
                buf |> Array.mapi(fun i b -> (IntKey i, IntVal(int32 b))) |> Map.ofArray |> TableVal
            else
                NoneVal

        let streamReadText (_ : NativeFunction<Value>) =
            if req.HasEntityBody then
                use reader = new IO.StreamReader(stream, req.ContentEncoding)
                let buf = reader.ReadToEnd()
                let result = buf |> StringVal
                reader.Close()
                stream.Close()
                result
            else
                NoneVal

        [
            (Key "canWrite", BoolVal stream.CanWrite);
            (Key "canRead", BoolVal stream.CanRead);
            (Key "read", NativeFunVal streamRead);
            (Key "readText", NativeFunVal streamReadText);
        ] |> Map.ofList |> TableVal

    let buildRequestTable (req:HttpListenerRequest) =
        let headers = tableOfNameValueCollection req.Headers
        let query = tableOfNameValueCollection req.QueryString
        let inputStream = requestStreamToTable req
        [
            (Key "inputStream", inputStream);
            (Key "method", StringVal req.HttpMethod);
            (Key "headers", headers);
            (Key "url", StringVal (req.Url.ToString()));
            (Key "query", query)
            (Key "path", StringVal req.Url.LocalPath)
        ] |> Map.ofList |> TableVal

    let buildResponseTable (res: HttpListenerResponse) =
        let setHeader (call : NativeFunction<Value>) =
            checkArgLength 2 call.arguments
            let headerKey = match call.arguments.Head with | StringVal s -> s | _ -> failwith "string expected"
            let headerValue = match call.arguments.Tail.Head with | StringVal s -> s | _ -> failwith "string expected"
            res.AddHeader(headerKey, headerValue)
            UnitVal
        let sendText (call : NativeFunction<Value>) =
            checkArgLength 1 call.arguments
            let text = match call.arguments.Head with | StringVal s -> s | _ -> failwith "string expected"
            res.OutputStream.Write(text |> Encoding.UTF8.GetBytes, 0, text.Length)
            res.OutputStream.Close()
            UnitVal

        let setStatus (call: NativeFunction<Value>) =
            checkArgLength 1 call.arguments
            let code = match call.arguments.Head with | IntVal i -> i | _ -> failwith "integer expected"
            res.StatusCode <- code
            UnitVal

        let send (call: NativeFunction<Value>) =
            checkArgLength 1 call.arguments
            match call.arguments.Head with
            | StringVal _ -> sendText call
            | TableVal bufferTable ->
                let buf : byte [] =
                    bufferTable 
                    |> Map.toArray
                    |> Array.filter(fun (k, v) ->
                        match (k, v) with
                        | IntKey _, IntVal value ->
                            if value < 0 || value > 255 then
                                failwith (sprintf "Invalid buffer value (range expected: 0-255, was: %i)" value)
                            true
                        | _ -> false
                    )
                    |> Array.map(fun (_, v) ->
                        match v with
                        | IntVal value -> (byte value)
                        | _ -> failwith "Invalid buffer value"
                    )
                res.OutputStream.Write(buf, 0, buf.Length)
                res.OutputStream.Close()
                UnitVal
            | _ -> failwith "[Http.response.send] buffer table or string expected"

        [
            (Key "setHeader", NativeFunVal setHeader);
            (Key "setStatus", NativeFunVal setStatus);
            (Key "sendText", NativeFunVal sendText);
            (Key "send", NativeFunVal send);
        ] |> Map.ofList |> TableVal

    let routeHttp (call : NativeFunction<Value>) = 
        checkArgLength 2 call.arguments
        let routeTemplate = getStringArg call.arguments.Head
        let routeRegex = RegularExpressions.Regex(@"\:([a-zA-Z][a-zA-Z0-9_]*)")
        let matches = routeRegex.Matches routeTemplate
        let matchPairs = matches |> List.ofSeq |> List.map(fun tmpl -> (tmpl, @"([^/\?\#\&]+)" ))
        let routePattern = matchPairs |> List.fold (fun (acc : string) (item, repl) -> acc.Replace(item.Value, repl)) routeTemplate
        let fullRegex = RegularExpressions.Regex(routePattern + "$")
        let url = getStringArg call.arguments.Tail.Head
        let routeMatches = fullRegex.Matches(url) |> List.ofSeq
        match routeMatches |> List.tryItem 0 with 
        | Option.None -> NoneVal
        | Some m -> 
            let parts = 
                m.Groups 
                |> Seq.cast<RegularExpressions.Group> 
                |> Seq.map(fun grp -> grp.Value)
                |> List.ofSeq
                |> List.tail
            let path = m.Value |> StringVal
            parts 
            |> List.mapi(fun i part ->
                let (key, _) = matchPairs |> List.item i
                (Key (key.Value.Replace(":", "")), part |> StringVal)
            )
            |> List.append [(Key "path", path)]
            |> Map.ofList |> TableVal

    let createServer (call : NativeFunction<Value>) =
        checkArgLength 2 call.arguments
        let host = match call.arguments.Head with | StringVal s -> s | _ -> failwith "string expected"
        match call.arguments.Tail.Head with
        | FunVal (_, parameters, body, fnScope) ->
            getListener host (fun req res -> async {
                let callArgs = 
                    parameters |> List.mapi(fun i p ->
                        match i with
                        | 0 -> (p, (buildRequestTable req))
                        | 1 -> (p, (buildResponseTable res))
                        | _ -> (p, NoneVal)
                    )
                    |> Map.ofList
                let callScope = Environment.joinMap fnScope.local callArgs
                call.eval { outer = fnScope.outer; local = callScope } body |> ignore
            })
            UnitVal
        | _ -> failwith "[Http.createServer] function expected"

    let httpTable = 
        [
            (Key "route", NativeFunVal routeHttp);
            (Key "createServer", NativeFunVal createServer)
        ] |> Map.ofList |> TableVal