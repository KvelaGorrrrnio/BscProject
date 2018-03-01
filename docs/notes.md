## Interpreters
1: First, decide on the format of the abstract syntax tree; how should our program be abstractly represented such that intepretation of the given language is as simple as possible?
2: After a format has been found, consider the parser and interpreting engine. These depend upon the format of the abstract syntax tree. How do we handle the different structures?
3: Write a lexer. Can be done whenever.

## Command line arguments and error handling
A nice thing would be handling command line arguments robustly - do we want an interactive shell or do we want to interpret a file? Do we need flags - to generate a log, to emulate a step-by-step execution of the program, whatever?
How do we elegantly handle errors in a way that makes sense?

Also, consider how to elegantly handle commands in the interactive shell.

## Common structures
a ::= x    op= e
    | x[e] op= e
    | x <=> x
    | push x x
    | pop  x x
    | skip

e ::= c | x | x[e] | e op' e | top x | empty x

x ::= variable (list | int)

c ::= unsigned 32bit int immediate

op ::= +
     | -
     | ^ (XOR)

op' ::= op | * | / | ...

OBS: A variable on the left-hand side of an = can't be on the right-hand side also.

## Inversion of reversible flowcharts
The article discusses how to invert reversible programs:

    1: change the direction of each arrow and exchange enty points and exit points (reverse order of instructions)
    2: replace each step function a with its inverse a^-1, nad
    3: replace each test by an assertion and each assertion by a test (the predicate e remains the unchanged).

## Translation between RL and SRL
Written in the article. We have to read it.
