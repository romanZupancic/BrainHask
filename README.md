# BrainHask
-------------------------

BrainHask is an interpreter for the esoteric programming language 
[Brainfuck](https://en.wikipedia.org/wiki/Brainfuck), written in Haskell.

## Features

BrainHask is a fully featured Brainfuck interpreter - it supports all eight (8) Brainfuck instructions, including: `+-><[],.`.

The operations `,` and `.` are forwarded through `stdin` and `stdout`, allowing interpreted Brainfuck programs full interactivity in alliance with other common implementations.

BrainHask itself includes some features that make running and inspecting code easy. Specifically, BrainHask includes:

### File interpreter

Expected of any basic interpreter, BrainHask can read in a Brainfuck source file and run it live.

```shell
> BrainHask [source.bf]
```

### Command-Line interpreting

Aside from just source files, BrainHask can read in Brainfuck code supplied to it as a command-line argument.

```shell
> BrainHask -c ",>,[<+>-]<."
```

### Interactive mode

BrainHask also includes an interactive REPL (Read-Eval-Print-Loop) that can initiate after running some Brainfuck source, or without any source at all.

The REPL also offers memory inspection after each sequence of program instructions

```shell
> BrainHask -i
~~| ++
[2]

~~| >>>+++
[2,0,0,3]
```

### Final Memory Mode

Inspecting the final state of the Brainfuck memory tape is easy with memory mode:

```shell
> BrainHask -mc "++++>+++++[<+>-]<."
9

Final Memory State:
[9,0]
```

## Interpreter Implementation Details

Because there is no single defined Brainfuck specification, this interpreter follows some common properties of other Brainfuck interpreters, but also makes its own design decisions.

Those are listed here:

### Memory Cell Value limits

While most common implementations of a Brainfuck interpreter allocate a single Byte per cell, each cell allocated by BrainHask is an integer: functionally, BrainHask supports a maximum value of 2147483647 (instead of 256) and a minimum of -2147483648 (instead of 0): the bounds of a signed integer.

> In short: Each memory cell allocated by BrainHask obeys the 32-Bit iteger limits, (3214748364 to -2147483648).

### Infinite Tape (Memory)

The limit of the Brainfuck memory tape is not set, but instead defined by your computer's usable memory. When a Brainfuck program begins, only one cell exists, and more are created as they are required (through the use of the `>` operator).

> In short: Brainfuck execution memory is theoretically infinite.

### Lenient Tape Start

The Tape memory is only infinite in the positively indexed direction. When attempting to move past index 0 of the tape memory in the negative direction (through `<`), instead of crashing, the interpreter will refuse to move the indexer. 

> In short: You can never move the tape pointer to a memory location before the start location.

### Integer and Character Input

Upon a read in instruction `,`, the interpreter will prompt for input. 
If the input can be read as an integer, it will be. Otherwise, the first character of input will be read in as it's ASCII value.

> In short: A `,` instruction will read in either an integer, or the first character of a string as it's ASCII value.