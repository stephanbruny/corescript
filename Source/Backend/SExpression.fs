namespace CoreScript.Backend

open CoreScript
open CoreScript.Parser
open CoreScript.Tokens
open CoreScript.Interpreter

module SExpression = 

    type SExpr = 
    | SXAtom of string
    | SXList of SExpr list
    | SXArgs of SExpr list
    | SXCall of SExpr * SExpr

    let getExpr (expr : SExpr list) =
        match expr with
        | [single] -> single
        | [] -> "unit" |> SXAtom
        | _ -> expr |> SXList

    let rec serialize (expressions : SExpr list) =
        expressions |> List.map(fun expr ->
            match expr with
            | SXAtom s -> s
            | SXList li -> li |> serialize
            | SXArgs li -> ["("; li |> serialize; ")"] |> String.concat ""
            | SXCall (left, right) -> ["("; ([left] |> serialize); " "; ([right] |> serialize); ")"] |> String.concat ""
        ) |> String.concat " "

    let rec transpile (results : SExpr list) (tokens : Token list) =
        match tokens with
        | [] -> results |> List.rev
        | head::tail ->
            match head with
            | Atom atom ->
                let atomExpr =
                    match atom with
                    | String s -> ["\""; s; "\""] |> String.concat "" |> SXAtom
                    | Integer i -> i.ToString() |> SXAtom
                    | Double d -> d.ToString() |> SXAtom
                    | Bool b -> b.ToString() |> SXAtom
                    | Unit -> "unit" |> SXAtom
                    | None -> "none" |> SXAtom
                    | SomeAtom -> "some" |> SXAtom
                    | Name n ->
                        match n with
                        | Simple sname -> sname |> SXAtom
                        | Subname slist -> slist |> String.concat "." |> SXAtom
                transpile (atomExpr::results) tail
            | Function (fnName, paramNames, body) ->
                let funName = fnName |> Environment.nameToString |> SXAtom
                let parameters = paramNames |> List.map (Environment.nameToString >> SXAtom) |> SXArgs
                let fnBody = transpile [] [body] |> SXList
                let defun = [ funName; parameters; fnBody ] |> SXList
                let result = SXCall ("defun" |> SXAtom, defun)
                transpile (result::results) tail
            | Lambda (paramNames, body) ->
                let parameters = paramNames |> List.map (Environment.nameToString >> SXAtom) |> SXArgs
                let fnBody = transpile [] [body] |> SXList
                let defun = [ parameters; fnBody ] |> SXList
                let result = SXCall ("lambda" |> SXAtom, defun)
                transpile (result::results) tail
            | Call (fn, callParams) ->
                let fnTarget = transpile [] [fn] |> getExpr
                let parameters = [callParams] |> transpile [] |> SXList
                let result = SXCall (fnTarget, parameters)
                transpile (result::results) tail
            | InfixOperator (op, left, right) ->
                let leftX = [left] |> transpile [] |> getExpr
                let rightX = [right] |> transpile [] |> getExpr
                let result = SXCall(op |> SXAtom, [leftX; rightX] |> SXList)
                transpile (result::results) tail
            | Prefix (op, right) ->
                let rightX = [right] |> transpile [] |> getExpr
                let result = SXCall(op |> SXAtom, rightX)
                transpile (result::results) tail
            | Let (left, right) ->
                let leftX = [left] |> transpile [] |> getExpr
                let rightX = [right] |> transpile [] |> getExpr
                let result = SXCall("define" |> SXAtom, [leftX; rightX] |> SXList)
                transpile (result::results) tail
            | Extend (left, right) ->
                let leftX = [left] |> transpile [] |> getExpr
                let rightX = [right] |> transpile [] |> getExpr
                let result = SXCall("extend" |> SXAtom, [leftX; rightX] |> SXList)
                transpile (result::results) tail
            | MatchCase (left, right) ->
                let leftX = [left] |> transpile [] |> getExpr
                let rightX = [right] |> transpile [] |> getExpr
                let result = SXCall("match-case" |> SXAtom, [leftX; rightX] |> SXList)
                transpile (result::results) tail
            | Match (left, right) ->
                let leftX = [left] |> transpile [] |> getExpr
                let rightX = right |> transpile [] |> SXList
                let result = SXCall("match" |> SXAtom, [leftX; rightX] |> SXList)
                transpile (result::results) tail
            | While (left, right) ->
                let leftX = [left] |> transpile [] |> getExpr
                let rightX = [right] |> transpile [] |> SXList
                let result = SXCall("while" |> SXAtom, [leftX; rightX] |> SXList)
                transpile (result::results) tail
            | TryCatch (left, right) ->
                let leftX = [left] |> transpile [] |> SXList
                let rightX = [right] |> transpile [] |> SXList
                let result = SXCall("try" |> SXAtom, [leftX; rightX] |> SXList)
                transpile (result::results) tail
            | Var (left, right) ->
                let leftX = [left] |> transpile [] |> getExpr
                let rightX = [right] |> transpile [] |> getExpr
                let result = SXCall("defvar" |> SXAtom, [leftX; rightX] |> SXList)
                transpile (result::results) tail
            | Access (left, right)
            | KeyAccess (left, right) ->
                let leftX = [left] |> transpile [] |> getExpr
                let rightX = [right] |> transpile [] |> getExpr
                let result = SXCall("get" |> SXAtom, [leftX; rightX] |> SXList)
                transpile (result::results) tail
            | Send (left, right) ->
                let leftX = [left] |> transpile [] |> getExpr
                let rightX = [right] |> transpile [] |> getExpr
                let result = SXCall("send" |> SXAtom, [leftX; rightX] |> SXList)
                transpile (result::results) tail
            | Mutate (left, right) ->
                let leftX = [left] |> transpile [] |> getExpr
                let rightX = [right] |> transpile [] |> getExpr
                let result = SXCall("setvar" |> SXAtom, [leftX; rightX] |> SXList)
                transpile (result::results) tail
            | Group tok -> 
                let result = tok |> transpile [] |> SXList
                transpile (result::results) tail
            | Block tok ->
                let result = tok |> transpile [] |> getExpr
                transpile (result::results) tail
            | If (clause, thenBody, elseBody) ->
                let ifClause = [clause] |> transpile [] |> SXList
                let ifBody = [thenBody] |> transpile [] |> getExpr
                let exprBody =
                    match elseBody with
                    | Some elBody ->
                        let el = [elBody] |> transpile [] |> getExpr
                        [ifBody; el] |> SXList
                    | _ -> ifBody
                let result = SXCall ("if" |> SXAtom, [ifClause; exprBody] |> SXList)
                transpile (result::results) tail
            | KeyValue (k, v) ->
                let tableKey =
                    match k with
                    | StringKey s
                    | OperatorKey s
                    | NameKey s -> ":" + s |> SXAtom
                    | NumericKey i -> ":" + (i.ToString()) |> SXAtom
                let value = [v] |> transpile [] |> getExpr
                let result = SXCall (tableKey, value)
                transpile (result::results) tail
            | MutableProperty (KeyValue (k, v)) ->
                let tableKey =
                    match k with
                    | StringKey s
                    | OperatorKey s
                    | NameKey s -> ":$" + s |> SXAtom
                    | NumericKey i -> ":$" + (i.ToString()) |> SXAtom
                let value = [v] |> transpile [] |> getExpr
                let result = SXCall (tableKey, value)
                transpile (result::results) tail
            | Receiver (KeyValue (k, v)) ->
                let tableKey =
                    match k with
                    | StringKey s
                    | OperatorKey s
                    | NameKey s -> "@" + s |> SXAtom
                    | _ -> failwith "Receiver name cannot be numeric"
                let value = [v] |> transpile [] |> getExpr
                let result = SXCall (tableKey, value)
                transpile (result::results) tail
            | Table contents ->
                let table = contents |> transpile [] |> getExpr
                let result = SXCall ("table" |> SXAtom, table)
                transpile (result::results) tail
            | Async tok ->
                let result = SXCall ("async" |> SXAtom, ([tok] |> transpile [] |> getExpr))
                transpile (result::results) tail
            | Await tok ->
                let result = SXCall ("await" |> SXAtom, ([tok] |> transpile [] |> getExpr))
                transpile (result::results) tail
            | Agent tok ->
                let result = SXCall ("await" |> SXAtom, ([tok] |> transpile [] |> getExpr))
                transpile (result::results) tail
            | Return tok ->
                let result = SXCall ("return" |> SXAtom, ([tok] |> transpile [] |> getExpr))
                transpile (result::results) tail
            | Reference tok ->
                let result = SXCall ("ref" |> SXAtom, ([tok] |> transpile [] |> getExpr))
                transpile (result::results) tail
            | Import tok ->
                let result = SXCall ("import" |> SXAtom, ([tok] |> transpile [] |> getExpr))
                transpile (result::results) tail
            | Export tok ->
                let result = SXCall ("export" |> SXAtom, ([tok] |> transpile [] |> getExpr))
                transpile (result::results) tail
            | Throw tok ->
                let result = SXCall ("throw" |> SXAtom, ([tok] |> transpile [] |> getExpr))
                transpile (result::results) tail
            | DefaultCase tok ->
                let result = SXCall ("default-case" |> SXAtom, ([tok] |> transpile [] |> getExpr))
                transpile (result::results) tail
            | _ -> transpile results tail


