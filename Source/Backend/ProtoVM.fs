namespace CoreScript.ProtoVM

open CoreScript
open CoreScript.Interpreter
open CoreScript.Interpreter.Environment
open CoreScript.Tokens
open CoreScript.Interpreter

open System.Collections.Generic

type OpCode =
| Push of Value
| Const of Value // Store a constant
| Var of Value
| Mut of int * Value
| MutA of int
| Store
| LoadC of int // Load a constant
| Call of int * (Value list -> Value) // Call Native Function (paramCount * args -> result)
| CallA of int
| CallR of int * int // Call Recursive of (address * param count)
| Defun of OpCode list
| CallFn of int // Call vm function by paramCount * address
| Pop
| PushPC of int
| Jmp of int
| JmpR of int
| JmpT of int
| JmpF of int
| Dup
| Add
| Sub
| Mul
| Div
| Mod
| Ret
| CmpEq
| CmpGt
| CmpLt
| CmpEqGt
| CmpEqLt
| CmpUeq
| Return
| Nop

module VirtualMachine = 

    let isTruthy (v : Value) =
        match v with
        | NoneVal -> false
        | BoolVal b -> b
        | IntVal i -> i <> 0
        | DoubleVal d -> d <> 0.0
        | _ -> true

    let writeDebug str = 
        async {
            System.Diagnostics.Debug.WriteLine str
            System.Diagnostics.Debug.Flush()
        } |> Async.Start

    let rec executeVm pc (stack : Stack<Value>) (opcodes : OpCode list) =
        let currentOp = opcodes |> List.tryItem pc
        // printfn "%A" currentOp
        match currentOp with
        | Some op ->
            match op with
            | Var _ 
            | Const _ -> executeVm (pc + 1) stack opcodes
            | Store ->
                let value = stack.Pop()
                let prog = opcodes |> Array.ofList 
                Array.set prog pc (Const value)
                executeVm (pc + 1) stack (prog |> List.ofArray)
            | Mut (address, value) ->
                match opcodes |> List.tryItem address with
                | Some (Var _) ->
                    let prog = opcodes |> Array.ofList 
                    Array.set prog address (Var value)
                    executeVm (pc + 1) stack (prog |> List.ofArray)
                | _ -> failwith "Invalid mutation"
            | MutA address ->
                let value = stack.Pop()
                match opcodes |> List.tryItem address with
                | Some (Var _) ->
                    let prog = opcodes |> Array.ofList 
                    Array.set prog address (Var value)
                    executeVm (pc + 1) stack (prog |> List.ofArray)
                | _ -> failwith "Invalid mutation"
            | LoadC i ->
                match opcodes |> List.tryItem i with
                | Some (Var v)
                | Some (Const v) ->
                    stack.Push v
                    executeVm (pc + 1) stack opcodes
                | _ -> failwith "Invalid constant address"
            | Push a -> 
                stack.Push a
                executeVm (pc + 1) stack opcodes
            | Pop -> 
                stack.Pop() |> ignore
                executeVm (pc + 1) stack opcodes
            | PushPC i ->
                stack.Push (IntVal (pc + i))
                executeVm (pc + 1) stack opcodes
            | Dup -> 
                stack.Push(stack.Peek())
                executeVm (pc + 1) stack opcodes
            | Jmp i -> 
                executeVm i stack opcodes
            | JmpR i -> executeVm (pc + i) stack opcodes
            | JmpT i ->
                let value = stack.Pop()
                if isTruthy value then
                    executeVm i stack opcodes
                else
                    executeVm (pc + 1) stack opcodes
            | JmpF i ->
                let value = stack.Pop()
                if not (isTruthy value) then
                    executeVm i stack opcodes
                else
                    executeVm (pc + 1) stack opcodes
            | Ret ->
                let value = stack.Pop()
                let returnAddress = stack.Pop()
                stack.Push value
                match returnAddress with
                | IntVal i -> executeVm i stack opcodes
                | _ -> failwith "Invalid return"
            | Add ->
                let b = stack.Pop()
                let a = stack.Pop()
                let result = Interpreter.plus a b
                stack.Push(result)
                executeVm (pc + 1) stack opcodes
            | Sub ->
                let b = stack.Pop()
                let a = stack.Pop()
                let result = Interpreter.minus a b
                stack.Push(result)
                executeVm (pc + 1) stack opcodes
            | Mul ->
                let b = stack.Pop()
                let a = stack.Pop()
                let result = Interpreter.multiply a b
                stack.Push(result)
                executeVm (pc + 1) stack opcodes
            | Div ->
                let b = stack.Pop()
                let a = stack.Pop()
                let result = Interpreter.divide a b
                stack.Push(result)
                executeVm (pc + 1) stack opcodes
            | Mod ->
                let b = stack.Pop()
                let a = stack.Pop()
                let result = Interpreter.modulo a b
                stack.Push(result)
                executeVm (pc + 1) stack opcodes
            | CmpEq ->
                let b = stack.Pop()
                let a = stack.Pop()
                let result = Environment.compareValues a b |> BoolVal
                stack.Push(result)
                executeVm (pc + 1) stack opcodes
            | CmpUeq ->
                let b = stack.Pop()
                let a = stack.Pop()
                let result = Environment.compareValues a b |> not |> BoolVal
                stack.Push(result)
                executeVm (pc + 1) stack opcodes
            | CmpGt ->
                let b = stack.Pop()
                let a = stack.Pop()
                let result = Interpreter.compareGrater a b |> BoolVal
                stack.Push(result)
                executeVm (pc + 1) stack opcodes
            | CmpLt ->
                let b = stack.Pop()
                let a = stack.Pop()
                let result = Interpreter.compareLess a b |> BoolVal
                stack.Push(result)
                executeVm (pc + 1) stack opcodes
            | CmpEqGt ->
                let b = stack.Pop()
                let a = stack.Pop()
                let result = Interpreter.compareGraterOrEqual a b |> BoolVal
                stack.Push(result)
                executeVm (pc + 1) stack opcodes
            | CmpEqLt ->
                let b = stack.Pop()
                let a = stack.Pop()
                let result = Interpreter.compareLessOrEqual a b |> BoolVal
                stack.Push(result)
                executeVm (pc + 1) stack opcodes
            | Call (argCount, fn) ->
                let args = [0..argCount] |> List.map(fun _ -> stack.Pop())
                let result = fn args
                stack.Push result
                executeVm (pc + 1) stack opcodes
            | CallA address ->
                let callStack : Stack<Value> = executeVm address stack opcodes
                let result = callStack.Pop()
                stack.Push result
                executeVm (pc + 1) stack opcodes
            | CallR (address, cnt) ->
                let callStack = new Stack<Value> ()
                [0..cnt] |> List.iter(fun _ -> callStack.Push(stack.Pop()))
                let resultStack = executeVm address callStack opcodes
                executeVm (pc + 1) resultStack opcodes
            | CallFn address ->
                match opcodes |> List.tryItem address with
                | Some (Defun fnOpcodes) ->
                    let next = executeVm 0 stack fnOpcodes
                    executeVm (pc + 1) next opcodes
                | _ -> failwith "Invalid call"
            | Return -> stack
            | Defun _
            | Nop -> executeVm (pc + 1) stack opcodes
        | _ -> failwith "Invalid program"

    