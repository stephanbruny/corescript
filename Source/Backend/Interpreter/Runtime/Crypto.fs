namespace CoreScript.Interpreter.RuntimeModules

open CoreScript.Interpreter
open CoreScript.Interpreter.Environment

open System
open System.Text
open System.Security.Cryptography

module Crypto =

    type CryptoHashProvider = {
        update : Value -> CryptoHashProvider;
        digest : Value -> Value;
    }

    let checkArgLength minLen (values: Environment.Value list) =
        if minLen > values.Length then
            failwith (sprintf "Insufficient arguments (expected: %i, given: %i)" minLen values.Length)

    let createHash (cryptoServiceProvider : HashAlgorithm, encoding : Encoding, digest : byte[] -> string) (input : string) = 
        let buf = encoding.GetBytes input
        let resultBytes = cryptoServiceProvider.ComputeHash buf
        digest resultBytes

    let createMd5Hash digest input =
        use md5 = new MD5CryptoServiceProvider()
        input |> createHash (md5, Encoding.UTF8, digest)

    let createSha256Hash digest input =
        use sha = new SHA256CryptoServiceProvider()
        input |> createHash (sha, Encoding.UTF8, digest)

    let createSha1Hash digest input =
        use sha = new SHA1CryptoServiceProvider()
        input |> createHash (sha, Encoding.UTF8, digest)

    let createSha512Hash digest input =
        use sha = new SHA512CryptoServiceProvider()
        input |> createHash (sha, Encoding.UTF8, digest)

    let getHash digest name =
        match name with
        | "md5" -> createMd5Hash digest
        | "sha1" -> createSha1Hash digest
        | "sha256" -> createSha256Hash digest
        | "sha512" -> createSha512Hash digest
        | _ -> failwith (sprintf "Unknown hash algorithm '%s'" name)

    let getDigest name = 
        match name with
        | "base64" -> Convert.ToBase64String
        | "hex" -> BitConverter.ToString
        | "utf8" -> Encoding.UTF8.GetString
        | "ascii" -> Encoding.ASCII.GetString
        | _ -> Encoding.Default.GetString

    let hashCrypto (call : NativeFunction<Value>) = 
        checkArgLength 2 call.arguments
        let algo = call.arguments.Head
        let format = call.arguments.Tail.Head
        match (algo, format) with
        | (StringVal hashName, StringVal digestName) ->
            let digest = getDigest digestName
            let hash = getHash digest hashName
            let hashFn (fnCall : NativeFunction<Value>) =
                checkArgLength 1 fnCall.arguments
                match fnCall.arguments.Head with
                | StringVal value ->
                    let result = hash value
                    StringVal result
                | _ -> failwith "string expected"
            hashFn |> NativeFunVal
        | _ -> failwith "string expected"

    let cryptoTable = 
        [
            (Key "createHash", NativeFunVal hashCrypto)
        ] |> Map.ofList |> TableVal