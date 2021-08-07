# BrainHask

BrainHask is an interpreter for the esoteric programming language 
[Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) (herein "Brainf***), written in Haskell.

## Features

BrainHask is a fully featured Brainf*** interpreter - it supports all eight (8) Brainf*** instructions, including: `+-><[],` and `.`.

The operations `,` and `.` are forwarded through `stdin` and `stdout`, allowing interpreted Brainf*** programs full interactivity with the system it is being run on.

BrainHask itself includes some features that make running and inspecting code easy. Specifically, BrainHask includes:

### File interpreter

Expected of any basic interpreter, BrainHask can read in a Brainf*** source file and run it live.

```shell
> BrainHask [source.bf]
```

### Command-Line interpreting

Aside from just source files, BrainHask can read in Brainf*** code supplied to it as a command-line argument.

```shell
> BrainHask -c ",>,[<+>-]<."
```

### Interactive mode

BrainHask also includes an interactive REPL (Read-Eval-Print-Loop) that can initiate after running some Brainf*** source, or without any source at all.

The REPL also offers memory inspection after each sequence of program instructions (through the `tm` REPL command).

```shell
> BrainHask -i
~~|++.
2

~~|:tm
|Memory| [2]

~~|>>>+++.
3
|Memory| [2,0,0,3]
```

### Final Memory Mode

Inspecting the final state of the Brainf*** memory tape is easy with memory mode:

```shell
> BrainHask -mc "++++>+++++[<+>-]<."
9

Final Memory State:
[9,0]
```

## Interpreter Implementation Details

Because there is no single defined Brainf*** specification, this interpreter follows some common properties of other Brainf*** interpreters, but also makes its own design decisions.

Those are listed here:

### - Memory Cell Value limits

While most common implementations of a Brainf*** interpreter allocate a single Byte per cell, each cell allocated by BrainHask is an integer: functionally, BrainHask supports a maximum value of 2147483647 (instead of 256) and a minimum of -2147483648 (instead of 0): the bounds of a signed integer.

> In short: Each memory cell allocated by BrainHask obeys the 32-Bit integer limits, (3214748364 to -2147483648).

### - Infinite Tape (Memory)

The limit of the Brainf*** memory tape is not set, but instead defined by your computer's usable memory. When a Brainf*** program begins, only one cell exists, and more are created as they are required (through the use of the `>` operator).

> In short: Brainf*** execution memory is theoretically infinite.

### - Lenient Tape Start

The Tape memory is only infinite in the positively indexed direction. When attempting to move past index 0 of the tape memory in the negative direction (through `<`), instead of crashing, the interpreter will refuse to move the indexer. 

> In short: You can never move the tape pointer to a memory location before the start location (0).

### - Integer and Character Input

Upon a read in instruction `,`, the interpreter will prompt for input. 
If the input can be read as an integer, it will be. Otherwise, the first character of input will be read in as it's ASCII value.

> In short: A `,` instruction will read in either an integer, or the first character of a string as it's ASCII value.