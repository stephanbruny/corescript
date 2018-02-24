namespace CoreScript.Interpreter

open System

open CoreScript
open Tokens

module Environment =

    type JTableKey = 
    | Key of string
    | IntKey of int32

    type ReceiverMessage<'T> = {
        subject: string;
        arguments: 'T list;
    }
    
    type Scope<'T> = {
        local : Map<string, 'T>;
        outer : Map<string ,'T>
    }

    type NativeFunction<'T> = {
        arguments: 'T list;
        scope: Scope<'T>;
        eval: Scope<'T> -> Token -> Scope<'T> * 'T
    }

    type Value =
    | StringVal of string
    | DoubleVal of double
    | IntVal of int32
    | BoolVal of bool
    | FunVal of string option * string list * Token * Scope<Value>
    | NativeFunVal of (NativeFunction<Value> -> Value)
    | TableVal of Map<JTableKey, Value>
    | ReceiverVal of Value // ReceiverVal of FunVal
    | AgentVal of Value * MailboxProcessor<ReceiverMessage<Value>> // Agent of TableVal
    | CollectionVal of Value list
    | RefVal of Name
    | Expression of Token
    | ErrorVal of string
    | ReturnVal of Value
    | Mutable of ref<Value>
    | Task of Value
    | AsyncVal of Async<Value>
    | SomeVal
    | NoneVal 
    | UnitVal


    type AgentTable = {
        table: Value;
        mailbox: MailboxProcessor<ReceiverMessage<Value>>;
    }

    let createAgent (table : Value) handler = 
        match table with
        | TableVal properties ->
            let receivers = properties |> Map.filter(fun _ value ->
                match value with
                | ReceiverVal _ -> true
                | _ -> false
            )
            let agentMailbox = MailboxProcessor<ReceiverMessage<Value>>.Start(fun inbox-> 
                let rec messageLoop() = async {
                    let! msg = inbox.Receive()
                    let receiver = receivers |> Map.tryFind (Key msg.subject)
                    do
                        match receiver with
                        | Some handle -> 
                            handler handle msg.arguments
                        | _ -> ()
                    return! messageLoop()  
                    }
                messageLoop()
                )
            (AgentVal(table, agentMailbox))
        | _ -> failwith (sprintf "Cannot create agent from %A (table required)" table)
       
    type Scope = Scope of Map<string, Value>

    let nameToString (token : Name) =
        match token with
        | Simple n -> n
        | Subname list -> list |> (String.concat ".")

    let rec getValue (scope : Scope<Value>) (token : Token) =
        let mapKeyValue (token : Token) =
            match token with
            | KeyValue (key, value) ->
                let tableKey = 
                    match key with
                    | NumericKey i -> IntKey i
                    | StringKey s -> Key s
                    | NameKey s -> Key s
                    | OperatorKey s -> Key s
                (tableKey, value |> getValue scope)
            | _ -> failwith (sprintf "Invalid token in table: %A" token)
        match token with
        | Atom (Atomic.String str) -> StringVal str
        | Atom (Atomic.Integer i) -> IntVal i
        | Atom (Atomic.Double d) -> DoubleVal d
        | Atom (Atomic.Bool b) -> BoolVal b
        | Atom (Atomic.Name n) -> RefVal n
        | Atom (Atomic.None) -> NoneVal
        | Atom (Atomic.Unit) -> UnitVal
        | Atom (Atomic.SomeAtom) -> SomeVal
        | Lambda (parameters, body) -> 
            FunVal (Option.None, (parameters |> List.map nameToString), body, scope)
        | Table list ->
            let tableMap = list |> List.map mapKeyValue |> Map.ofList
            TableVal tableMap
        | _ -> Expression token

    // let createLetBinding (scope : Map<string, Value>) (token : Token) =
    //     match token with
    //     | Let( name, value ) ->
    //         let targetName =
    //             match name with
    //             | Atom (Name (Simple n) ) -> n
    //             | Atom (Name (Subname list) ) -> list |> (String.concat ".")
    //             | _ -> failwith (sprintf "Invalid name in let binding (%A)" name)
    //         scope |> Map.add targetName (value |> getValue scope)
    //     | _ -> failwith (sprintf "Cannot create let binding from %A" token)

    let joinMap (p:Map<'a,'b>) (q:Map<'a,'b>) = 
        Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])

    let getIntArg (value : Value) = 
        match value with
        | IntVal i -> i
        | _ -> failwith "integer expected"

    let getStringArg (value : Value) = 
        match value with
        | StringVal str -> str
        | _ -> failwith "string expected"

    let getTableArg (value : Value) = 
        match value with
        | TableVal tab -> tab
        | _ -> failwith "table expected"

    // let getCallParams (scope : Map<string, Value>) (token : Token) =
    //     match token with
    //     | Group values -> values |> List.map (getValue scope)
    //     | Atom Unit -> []
    //     | _ -> failwith (sprintf "Invalid arguments: %A" token)

    let getCallName (token : Token) =
        match token with
        | Atom (Name n) -> n |> nameToString
        | _ -> failwith (sprintf "Invalid call: %A" token)

    let compareValues a b =
        match (a, b) with
        | (StringVal x, StringVal y) -> x = y
        | (IntVal x, IntVal y) -> x = y
        | (DoubleVal x, DoubleVal y) -> x = y
        | (IntVal x, DoubleVal y) -> (double x) = y
        | (DoubleVal x, IntVal y) -> x = (double y)
        | (BoolVal x, BoolVal y) -> x = y
        | (FunVal _, FunVal _) -> true // TODO
        | (NativeFunVal _, NativeFunVal _) -> true // TODO
        | (NoneVal, NoneVal) -> true
        | (UnitVal, UnitVal) -> true
        | (SomeVal, NoneVal) -> false
        | (NoneVal, SomeVal) -> false
        | (SomeVal, _) -> true
        | (_, SomeVal) -> true
        | _ -> false

    let rec compareTables a b =
        match (a, b) with
        | (TableVal tableA, TableVal tableB) ->
            let compareKeyValue (k : JTableKey, value : Value) (subject : Map<JTableKey, Value>) =
                let subjectKey = subject |> Map.tryFind k
                match subjectKey with
                | Option.None -> false
                | Some (TableVal t) -> compareTables value (TableVal t)
                | Some subjectValue -> compareValues value subjectValue
            let maybeDiff =
                tableA 
                |> Map.toArray 
                |> Array.tryFind(fun (k, v) -> not (compareKeyValue (k, v) tableB))
            match maybeDiff with
            | Option.None -> true
            | Some _ -> false
        | _ -> false