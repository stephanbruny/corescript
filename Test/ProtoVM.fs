namespace CoreScript.Test

open NUnit.Framework
open FsUnitTyped

open CoreScript.ProtoVM
open CoreScript.Interpreter.Environment

open System.Collections
open CoreScript.Interpreter

module ProtoVMTests = 

    let rec assertValue expected value =
        let eq a b = b |> shouldEqual a
        match (expected, value) with
        | (StringVal a, StringVal b) -> eq a b
        | (IntVal a, IntVal b) -> eq a b
        | (BoolVal a, BoolVal b) -> eq a b
        | (DoubleVal a, DoubleVal b) -> eq a b
        | (TableVal a, TableVal b) -> 
            a |> Map.iter(fun k v ->
                let bVal = b |> Map.tryFind k
                match bVal with
                | Some vb -> assertValue v vb
                | None _ -> failwith (sprintf "Expected key %A to be found, but was empty" k)
            )
        | (ErrorVal a, ErrorVal b) -> eq a b
        | (ReturnVal a, ReturnVal b) -> assertValue a b
        | (NoneVal, NoneVal) -> ()
        | (UnitVal, UnitVal) -> ()
        | _ -> failwith (sprintf "Expected %A, but was: %A" expected value)

    [<Test>]
    let ``should execute a simple AVM program`` () = 
        let stack = new Generic.Stack<Value> ()
        let program = 
            [
                OpCode.Const (IntVal 9)
                OpCode.Push (IntVal 3)
                OpCode.Push (IntVal 6)
                OpCode.Add
                OpCode.LoadC 0;
                OpCode.Mul;
                OpCode.Return;
            ]
        let res = VirtualMachine.executeVm 0 stack program
        res.Pop () |> (assertValue (IntVal 81))
        ()

    [<Test>]
    let ``should execute a conditional jump`` () = 
        let stack = new Generic.Stack<Value> ()
        let program = 
            [
                OpCode.Const (IntVal 9)
                OpCode.Push (IntVal 3)
                OpCode.Push (IntVal 6)
                OpCode.Add
                OpCode.LoadC 0;
                OpCode.Mul;
                OpCode.Push (IntVal 60);
                OpCode.CmpGt;
                OpCode.JmpT 16;
                OpCode.Push (StringVal "not here");
                OpCode.Nop;
                OpCode.Nop;
                OpCode.Nop;
                OpCode.Push (StringVal "also wrong");
                OpCode.Nop;
                OpCode.Nop;
                OpCode.Push (StringVal "correct");
                OpCode.Return;
            ]
        let res = VirtualMachine.executeVm 0 stack program
        printfn "%A" res
        res.Count |> shouldEqual 1
        res.Pop() |> (assertValue (StringVal "correct"))
        ()

    [<Test>]
    let ``should execute a conditional loop`` () = 
        let stack = new Generic.Stack<Value> ()
        let program = 
            [
                OpCode.Push (IntVal 0);
                OpCode.Push (IntVal 1);
                OpCode.Add;
                OpCode.Dup;
                OpCode.Push (IntVal 10);
                OpCode.CmpEqLt;
                OpCode.JmpT 1;
                OpCode.Return;
            ]
        let res = VirtualMachine.executeVm 0 stack program
        printfn "%A" res
        res.Count |> shouldEqual 1
        res.Pop () |> (assertValue (IntVal 11))
        ()

    [<Test>]
    let ``should execute a conditional loop (recursive function style)`` () = 
        let stack = new Generic.Stack<Value> ()
        let program = 
            [
                OpCode.Jmp 8; // jump over function
                // fn (i) { i + 1; if (i <= 10) fn(i); return i; }
                OpCode.Push (IntVal 1);  // function starts here...push 1
                OpCode.Add; // (i + 1)
                OpCode.Dup; // duplicate current value for next iteration
                OpCode.Push (IntVal 1000); // push 1000
                OpCode.CmpEqLt; // i <= 1000
                OpCode.JmpT 1; // recurse
                OpCode.Ret; // Return to address
                OpCode.Push (IntVal 11); // Push return address
                OpCode.Push (IntVal 0); // Push initial value
                OpCode.Jmp 1; // Jump into function
                OpCode.Return;
            ]
        let res = VirtualMachine.executeVm 0 stack program
        printfn "%A" res
        res.Count |> shouldEqual 1
        res.Pop () |> (assertValue (IntVal 1001))
        ()

    [<Test>]
    let ``should call native function`` () = 
        let stack = new Generic.Stack<Value> ()
        
        let fnMul (values : Value list) =
            let result = values |> List.fold(fun acc v -> Interpreter.multiply acc v) (IntVal 1)
            result

        let program = 
            [
                OpCode.Push (IntVal 4);
                OpCode.Push (IntVal 8);
                OpCode.Call (1, fnMul);
                OpCode.Return;
            ]
        let res = VirtualMachine.executeVm 0 stack program
        printfn "%A" res
        res.Count |> shouldEqual 1
        res.Pop () |> (assertValue (IntVal 32))
        ()

    [<Test>]
    let ``should call vm function`` () = 
        let stack = new Generic.Stack<Value> ()
        
        let addFn = 
            [
                OpCode.Add;
                OpCode.Return;
            ]

        let program = 
            [
                OpCode.Defun addFn;
                OpCode.Push (IntVal 4);
                OpCode.Push (IntVal 8);
                OpCode.CallFn 0;
                OpCode.Return;
            ]
        let res = VirtualMachine.executeVm 0 stack program
        printfn "%A" res
        res.Count |> shouldEqual 1
        res.Pop () |> (assertValue (IntVal 12))
        ()

    [<Test>]
    let ``should store a value`` () = 
        let stack = new Generic.Stack<Value> ()
        
        let program = 
            [
                OpCode.Push (StringVal "foo");
                OpCode.Store;
                OpCode.LoadC 1;
                OpCode.Return;
            ]
        let res = VirtualMachine.executeVm 0 stack program
        printfn "%A" res
        res.Count |> shouldEqual 1
        res.Pop () |> (assertValue (StringVal "foo"))
        ()

    [<Test>]
    let ``should store a value returned from a function`` () = 
        let stack = new Generic.Stack<Value> ()

        let fooFn = 
            [
                OpCode.Push(StringVal "FooBar");
                OpCode.Return;
            ]
        
        let program = 
            [
                OpCode.Defun fooFn;
                OpCode.CallFn 0;
                OpCode.Store;
                OpCode.LoadC 2;
                OpCode.Return;
            ]
        let res = VirtualMachine.executeVm 0 stack program
        printfn "%A" res
        res.Count |> shouldEqual 1
        res.Pop () |> (assertValue (StringVal "FooBar"))
        ()

    [<Test>]
    let ``should mutate a variable`` () = 
        let stack = new Generic.Stack<Value> ()
        
        let program = 
            [
                OpCode.Var (StringVal "");
                OpCode.Mut (0, (StringVal "Foo"));
                OpCode.LoadC 0;
                OpCode.Mut (0, (StringVal "Bar"));
                OpCode.LoadC 0;
                OpCode.Return;
            ]
        let res = VirtualMachine.executeVm 0 stack program
        printfn "%A" res
        res.Count |> shouldEqual 2
        res.Pop () |> (assertValue (StringVal "Bar"))
        res.Pop () |> (assertValue (StringVal "Foo"))
        ()

    [<Test>]
    let ``should call local function`` () = 
        let stack = new Generic.Stack<Value> ()
        
        let program = 
            [
                OpCode.Jmp 7;
                // getFoo ()
                OpCode.Push (IntVal 42);
                OpCode.Return;

                // x2 ()
                OpCode.CallA 1;
                OpCode.Dup;
                OpCode.Add;
                OpCode.Return;

                OpCode.CallA 3;
                OpCode.Return;

            ]
        let res = VirtualMachine.executeVm 0 stack program
        printfn "%A" res
        res.Count |> shouldEqual 1
        res.Pop () |> (assertValue (IntVal 84))
        ()

    [<Test>]
    let ``should call local recursive function`` () = 
        let stack = new Generic.Stack<Value> ()
        
        let program = 
            [
                OpCode.Jmp 20;
                // fib (n)
                OpCode.Var (IntVal 0); // n
                OpCode.MutA 1
                OpCode.LoadC 1; // n
                OpCode.Dup; // n, n
                OpCode.Push (IntVal 2); // n, n, 2
                OpCode.CmpLt; // n, true/false
                OpCode.JmpT 19; //  n
                OpCode.Push (IntVal 1); // n, 1
                OpCode.Sub; // (n - 1)
                OpCode.CallR (1, 0); // fib(n - 1)
                OpCode.Var (IntVal 0); // fub(n - 1)
                OpCode.MutA 11; // n
                OpCode.LoadC 1;
                OpCode.Push (IntVal 2) // n , 2
                OpCode.Sub; // (n - 2)
                OpCode.CallR (1, 0); // fib(n - 2)
                OpCode.LoadC 11;
                OpCode.Add;
                OpCode.Return;

                OpCode.Push (IntVal 30)
                OpCode.CallA 1;
                OpCode.Return;

            ]
        let res = VirtualMachine.executeVm 0 stack program
        printfn "%A" res
        res.Count |> shouldEqual 1
        res.Pop () |> (assertValue (IntVal 832040))
        ()