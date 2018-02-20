[<< Previous](./types.md) | [Index](./index.md) | [Next >>](./tables.md)

# Variables

## Let-Binding

A let-binding is a name *bound* to an expression or value.  
It is immutable: its value cannot be changed. (there isn't even any syntax to change a let-bindings value)

```corescript
let x = 10;
let foo = "bar";
let fn = fun () print("fn called.");
```

## Variables

A variable is a name connected to a mutable value.  
The value **and type** of a variable can be changed at any time.  

```corescript
var myVariable = 3;
myVariable <- myVariable + 3; // now containing 6
myVariable <- toString( myVariable ); // now containing "6"
```

Note the syntax:  
Var- or let-bindings are initialized with `=`-operator.  
To change the value of a variable, the mutation-operator `<-` has to be used.  
*Trying to apply the mutation-operator on a let-binding will lead to a runtime-error.*


## References

When using variables or let-bindings it is important to know, that CoreScript uses Call-By-Value everywhere.  
Calling a function will copy the values from variables and bind them to the functions scope.  
Working with mutables could lead to unexpected beheviors, if the mutable has to be modified from within a function.
This also has to be considered for tables and especially agents.  

To access mutables directly one must use the `ref` keyword:

```corescript
var count = 0;

function increaseCount(x) {
    x <- x + 1;
}

increaseCount(count); // "Runtime Error: Cannot mutate immutable" - because value 0 is copied
increaseCount(ref count); // count now contains 1

```

## Allowed names

Lexer-definition: `[a-zA-Z_][a-zA-Z0-9_]*`

A name must begin with a letter or underscore, and can be followed by letters, numbers or more underscores.  
It must not reassemble a CoreScript-keyword.  
The same rules apply to function names.

## Initialization

All names must be initialized immediately.  
Both var- and let-bindings require an assignment operation to be legal.

## Scoping

All names are lexically scoped. No hoisting.