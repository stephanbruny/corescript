namespace CoreScript

open System.Text.RegularExpressions
open System

module Lexer =
    let (|RegexMatch|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    type Token =
    | TName of string
    | TInteger of int
    | TDouble of double
    | TString of string
    | TSpace of string
    | TPunct of string
    | TOperator of string
    | TOpenBrace
    | TCloseBrace
    | TOpenBracket
    | TCloseBracket
    | TOpenCurly
    | TCloseCurly
    | TNewLine
    | TComment
    | TExpression of Token list
    | TEnd

    let getInteger str = Int32.Parse(str)

    let getDouble str = Double.Parse(str, Globalization.CultureInfo.InvariantCulture)

    let getStringContent (str : string) = str.Substring(1, str.Length - 2)

    let getToken (snippet: string) =
        match snippet with
        | "" -> (TEnd, String.Empty)
        // | RegexMatch @"^([\r\n|\n]+)" [tok] -> (TNewLine, tok)
        | RegexMatch @"^(\/\/.*)[$|\n]*" [tok] -> (TComment, tok)
        | RegexMatch @"^(/\*(?:(?!\*/)(?:.|[\r\n]+))*\*/)" [tok] -> (TComment, tok)
        | RegexMatch @"^(\s+)" [tok] -> (TSpace(tok), tok)
        | RegexMatch @"^(\-?\d+\.\d+)" [tok] -> (TDouble(tok |> getDouble), tok)
        | RegexMatch @"^(\-?\d+)" [tok] -> (TInteger(tok |> getInteger), tok)
        | RegexMatch @"^(\()" [tok] -> (TOpenBrace, tok)
        | RegexMatch @"^(\))" [tok] -> (TCloseBrace, tok)
        | RegexMatch @"^(\[)" [tok] -> (TOpenBracket, tok)
        | RegexMatch @"^(\])" [tok] -> (TCloseBracket, tok)
        | RegexMatch @"^(\{)" [tok] -> (TOpenCurly, tok)
        | RegexMatch @"^(\})" [tok] -> (TCloseCurly, tok)
        | RegexMatch @"^(\'[^\']*\')" [tok] -> (TString(tok |> getStringContent), tok)
        | RegexMatch @"^(\""[^\""]*\"")" [tok] -> (TString(tok |> getStringContent), tok)
        | RegexMatch @"^([a-zA-Z][a-zA-Z0-9_]*)" [tok] -> (TName(tok), tok)
        | RegexMatch @"^([\,\;\.\:])" [tok] -> (TPunct(tok), tok)
        | RegexMatch @"^([^\w\s\(\)]+)" [tok] -> (TOperator(tok), tok)
        | _ -> (TEnd, String.Empty)

    let rec execute result text line =
        let (token, part) = getToken text
        let newLines = part |> String.filter (fun c -> c = '\n')
        let nextLine = line + newLines.Length
        match token with
        | TEnd -> result |> List.filter(fun (token, _) -> match (token) with | TSpace _ -> false | _ -> true) |> List.rev
        | _ ->
            let len = Math.Max(part.Length, 1)
            let next = text.Substring(len)
            if token = TNewLine then
                execute (result) next (nextLine)
            else
                execute ((token, nextLine)::result) next nextLine