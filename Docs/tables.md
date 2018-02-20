[<< Previous](./variables.md) | [Index](./index.md) | [Next >>](./functions.md)

# Tables

Tables are container types, similar to associative arrays, storing key-values pairs.

## Creating Tables

Constructing a table can be done when initializing a let-binding or variable.  
Curly brackets `{ }` sorround the key-value-pairs `key: value`:  

```corescript
let MyTable = {
    foo: 'bar',
    'bar': 'foo',
    1: 1,
    2: 42.42
}
```

Allowed keys are *names*, *strings* and *integers*.  

## Mutable properties

Tables can contain mutable properties with the `mutable-syntax`:

```corescript
let MyTable = {
    foo: 'bar', // immutable
    mutable bar: 'foo'
}
```

## Accessing table properties

Named keys (or string keys following the name-syntax) can be accessed via `.`-notation: 

```corescript
print(MyTable.foo); // will print: "bar"
```

String-keys and integer-keys can be accessed via `[]`-notation: 

```corescript
print(MyTable[1]);
print(someTable['some key']);
```

This notation allows to use expressions to form a table key:

```corescript
// Assuming we have an array 'MyArray'
print(MyArray[MyArray.length - 1]);
```

## Mutation

Mutable table properties, can be modified:

```corescript
MyTable.bar <- "bar";
```

## Extend

Tables can be *extended* - creating a copy of the table with additional properties: 

```corescript
let FooTable = {
    foo: 'foo'
};

let BarTable = extend FooTable {
    bar: 'bar'
}
```

Different from an *inheritance*-approach, the new table contains all values from the table it was extended from, instead of references.  
That means, extensive usage of `extend` can have quite an impact on memory consumption.

---

See also

- [table (Runtime)](./runtime/table.md)
- [array (Runtime)](./runtime/array.md)