[Index](./index.md) | [Next >>](./getting-started.md)

# Introduction

> A dynamically typed, functional-first, object-oriented, concurrent, general-purpose, embeddable scipting and programming language 

Dynamic languages are appreciated for their expressiveness and ease of use.  
Functional programming can add alot of safety and improved maintainablility to applications written in 
such languages.  
With dotnet core (2.0), Microsoft developed a rich, open and portable platform for nearly every software project.  
Yet, .NET (core) is lacking a nice, quickly usable, dynamic scripting or programming language.  
CoreScript fills this gap -  a scripting language for dotnet core.  

CoreScript is heavily influenced by JavaScript, Lua and OCaml/F#.  
Although putting most emphasis on functional programming, the syntax should be familiar for developers coming from other
dynamic languages.  
It is designed for every-day-work, simplicity and clearity.  

CoreScript features immutability by default, dynamic typing, asynchronous functions 
and concurrent programming via Actor Model.  
It is multi-paradigm, and supports imperative, functional and object-oriented styles.
*Like in Lua, objects are called "tables" and are prototype-oriented.*  

Currently CoreScript is fully interpreted and therefor somewhat slow.  
In future, the language will be compiled with a just-in-time (JIT) approach to improve performance.  
Also it will be possible to create standalone executables.

**Important:** Dotnet core is not required to run CoreScript-files. The pre-built binary comes self-contained.