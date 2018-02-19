[<< Previous](./agents.md) | [Index](./index.md) | [Next >>](./extensions.md)

# Modules

Every CoreScript-File can be considered a *module*.  
Scripts are evaluated as *blocks* and therefor the same rules apply:
The last expression or the first evaluated `return` is used as result. 

## Importing 

```corescript
// other.cor
let api = {
    foo: fun () 'foo',
    bar: fun () 'bar'
};

export api;

// some other script file
let fs = import 'filesystem';
let otherModule = import './other.cor';
print(otherModule.foo()); // prints "foo"
```

The `export`-keyword is optional, but should be used to make clear, what is intended to be exported from the module.  
Also, keep the block-rules in mind, to not accidently return something unwanted from your module, just because it was
the last expression in your file.
The last expression in a module-script should always be `export`.