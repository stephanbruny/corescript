namespace CoreScript

module Tokens = 

    type Name =
    | Simple of string
    | Subname of string list

    type Atomic = 
    | String of string
    | Integer of int32
    | Double of double
    | Name of Name
    | Bool of bool
    | Unit
    | None

    type TableKey =
    | NameKey of string
    | StringKey of string
    | NumericKey of int32
    | OperatorKey of string

    type Token =
    | Atom of Atomic
    | Call of Token * Token // Call of Name/Function/Lambda * Arguments
    | Group of Token list
    | Function of Name * (Name list) * Token // Function of Name * Parameters * Body
    | Lambda of (Name list) * Token
    | Operator of string
    | InfixOperator of string * Token * Token // Operator of Op * Left Token * (Right Token)
    | Prefix of string * Token
    // | Postfix of string * Token
    | CustomOperator of string
    | OperatorDefinition of string * Token
    | Let of Token * Token // Let of Name/Group/Decomposition * Expression/Call/Function/...
    | Var of Token * Token // Var of Name/Group/Decomposition * Expression/Call/Function/...
    | Mutate of Token * Token // Mutate of FieldName * Expression (e.g. var xyz; xyz <- 123;)
    | Collection of Token list
    | Access of Token * Token
    | KeyAccess of Token * Token // e.g. something[foo]
    | Table of Token list
    | Return of Token
    | Agent of Token
    | Reference of Token
    | If of Token * Token * Token option // If of Condition * IfCase * ElseCase?
    | Else of Token
    | While of Token * Token
    | MatchCase of Token * Token
    | DefaultCase of Token
    | Case of Token
    | Match of Token * Token list // Match of Applicative * MatchCase list
    | Foreach of Token * Token // foreach of Condition * Body
    | Repeat of Token
    | Enum of Token list
    | Break
    | TryCatch of Token * Token // TryCatch of TryBlock * CatchBlock
    | Catch of Token * Token
    | Throw of Token
    | Block of Token list
    | Expression of Token list
    | Import of Token
    | Export of Token
    | MutableProperty of Token
    | Send of Token * Token // Send of ReceiverName * Call
    | Extend of Token * Token
    | Receiver of Token
    | KeyValue of TableKey * Token
    | As of Token * Token
    | Async of Token
    | Await of Token
    | RuntimeError of string
    | ParserError of string
    | Intermediate of Lexer.Token

    let getName token =
        match token with
        | Atom (Name name) -> name
        | _ -> failwith (sprintf "Expected name but got %A" token)

    let groupToNames token =
       match token with
       | Group list -> list |> List.map getName
       | _ -> failwith (sprintf "Expected Group but got %A" token)

    let parserErrorMessage message line =
        sprintf "Parser Error at Line %i: %s" line message

    let parseError message line =
        ParserError (sprintf "Parser Error at Line %i: %s" line message)