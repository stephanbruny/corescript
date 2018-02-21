[<< Previous](./introduction.md) | [Index](./index.md) | [Next >>](./types.md)

# Getting Started

To get into the language, follow the [build instructions](../README.md) and take a look into the  
[example scripts](../Source/Script).  

If you're already familiar with dotnet core, compiling and building is straight-forward.  

Use the REPL (Read-Eval-Print-Loop) by starting CoreScript without any arguments.  
This will allow you to run scripts, directly entered from your console and play with the language.
Exit the REPL with `ctrl + c`.


When you are interested in programing language implementation, you might also want to check the [source files](../Source).  
CoreScript is written in F# and uses a recursive descend approach for parsing.

## Usage

Published self-contained:

```
corescript - start REPL
corescript <file> - run script file
```

dotnet run:

```
dotnet run - start REPL
dotnet run <file> - run script file
```

There are no flags currently.