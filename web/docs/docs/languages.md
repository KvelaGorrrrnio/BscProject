# Reversible Flowchart Languages

The languages are based upon *Reversible Flowcharts* and thus are called *Reversible Flowchart Languages*.
RL (**R**eversible **L**angauge) and SRL (**S**tructured **R**eversible **L**anguage) can be split into 3 parts: A common part which, handles e.g. the step operations (instructions) and expressions, a RL specific part and a SRL specific. See the following sections for information regarding these.

## Common
The common part handles the parts that are common to both RL and SRL. This includes variable declarations, step operations and expresssions.

### Variable declarations

For both languages only two types are present: `int` and `list t`, where `t` is a recursive reference to either `int` or `list t`.
Both languages use type declarations for controlling variable types. The variable declarations can only be specified at the beginning of a program, e.g.:

    list int a      // 1-dimensional list
    list list int b // 2-dimensional list
    int c           // scalar integer variable

### Step operations
<dl>
`[var] [op]= [exp]`
  <dd>
  The variable update. Each declared variable has an initial value of `0`, if it is a scalar, or `[]`, if it is a list. Update operators must be injective. Four operators are supported:

  | Operator      | Functionality  | Inverse |
  | :-----------: |:--------------:| :-----: |
  | `+`           | Addition       | `-`     |
  | `-`           | Subtraction    | `+`     |
  | `^`           | Bitwise XOR    | `^`     |
  | `*`           | Multiplication | `/`     |
  | `/`           | Division       | `*`     |


  **Examples**: `a ^= 5`, `l[4] += 10`, `l[1,a+1] -= 3`

  **Rules**:

  - The variable being updated must not occur in its own index or in the expression on the right-hand side.
  - Division updates must not have a remainder.
  - The right-hand expression on multiplication updates must not be zero.
  </dd>

  ---

`push [var] [var]`
  <dd>
  Pushes the first operand onto the second operand. The first operand becomes zero-cleared afterwards.

  **Inverse**: `pop`

  **Examples**: `push a b`, `push l[1] s`

  **Rules**: The variable name in one operand must not occur in the other operand nor in itself. For instance, `push l[l[2]] b`, `push a b[a]`, etc. are not allowed.
  </dd>

  ---

`pop [var] [var]`
  <dd>
  Pops the first element of the second operand into the first operand.

  **Inverse**: `push`

  **Examples**: `pop a b`, `pop l[1] s`

  **Rules**: Same as `push`. Furthermore, the first operand must be zero-cleared beforehand.
  </dd>

  ---

`skip` or `.`
  <dd>
  Does nothing.

  **Inverse**: `skip`
  </dd>

  ---

`swap [var] [var]`
  <dd>
  Swap two variables.

  **Inverse**: `swap`

  **Examples**: `swap a b`, `swap l[1] s`, `swap l[2] l[5]`

  **Rules**: None.
  </dd>

  ---

`init [var] [dimensions]`
  <dd>
  Initialise a list of zero-values by providing one or more comma-separated depths, each corresponding to an additional dimension.

  **Inverse**: `free`

  **Examples**: `init l [10]`, `init b [2,5]`, `init c [5,2,6]`

  **Rules**: The list operand must be empty beforehand and the number of dimensions must match the type of the list.
  </dd>

  ---

`free [var] [dimensions]`
  <dd>
  Free a list by providing its specific depths.

  **Inverse**: `init`

  **Examples**: `free l [10]`, `free b [2,5]`, `free c [5,2,6]`

  **Rules**: The list must contain only zero-values, and the depths of the list must be exactly the same as the ones provided.
  </dd>

</dl>

### Expressions
Expressions do not have to be injective. Thus, we can hypothetically support any arbitrary operator. The binary operators currently supported are

| Operator      | Functionality |
| :-----------: |:-------------:|
| `+`           | Addition |
| `-`           | Subtraction |
| `^`           | Bitwise XOR |
| `*`           | Multiplication |
| `/`           | Division |
| `%`           | Modulo |
| `**`          | Power |
| `==`          | Equal to |
| `!=`          | Not equal to |
| `<`           | Less than |
| `<=`          | Less than or equal to |
| `>`           | Greater than |
| `>=`          | Greater than or equal to |
| `&&` or `and` | Logical conjunction |
| `||` or `or`  | Logical disjunction |

The unary operators currently supported are

| Operator       | Functionality |
| :------------: |:-------------:|
| `-` or `neg`   | Arithmetic negation |
| `~` or `sig`   | The sign function |
| `!` or `not`   | Logical negation |
| `null`         | Does the operand consist of only zero values? |
| `#` or `size`  | Size of a list |
| `?` or `empty` | Is the operand empty? |
| `^` or `top`   | Top of the operand (first element) |

Note that boolean values are represented as integers: 0 is evaluated to false and everything else is evaluated to true. If a logical express, e.g. `a && b`, evaluates to true, the resulting value is 1. If not, the resulting value is 0.

Furthermore, expressions support indexing. That is, we do not need to index directly on varibale names:
```
(top l)[5]
```

## RL
### Structure of RL
An RL program consists of one or more RL blocks. Each block has four components:

  - a label identifying the block
  - a come-from assertion
  - zero or more step operations
  - a jump

The job of the come-from assertion is to establish the origin of the control flow. This ensures deterministic backward execution and thus reversibility.

This gives us the structure of a block:

```
[label]: [come-from assertion]
  [step operations]
  [jump]
```

Jumps and come-from assertions can either be conditional, unconditional or mark the entry or exit of a program, respectively. That is,

*Come-from assertions*

  - `entry`
- `from [label]`
- `fi [exp] [label] [label]`

*Jumps*

  - `goto [label]`
- `if [exp] [label] [label]`
- `exit`

If the expression in a conditional come-from assertion evaluates to true, control flow should have been passed from the first label. Otherwise, it should have been passed from the second. Intuitively, if a block can be reached from two different places in the program, it should have a conditional come-from assertion that uniquely determines the exact origin at the given instance.\
\ \ \ \ For the program to be well formed, it has to have exactly one entry and one exit. Furthermore, the entry has to be the first block in the program, and the exit has to be the last.

### RL example
The following program will compute the $n^{th}$ Fibonacci pair reversibly.

```
// Compute the n'th fibonacci pair

int n
int v
int w

start: entry
  n ^= 16
  w ^= 1
goto loop

loop: fi (v = 0) start loop
  v += w
  swap v w
  n -= 1
if (n = 0 || v > w) end loop

end: from loop
exit
```

## SRL
### Structure of SRL
An SRL program consists of one or more SRL blocks. An SRL block can either be a step operation as described in [Common](#common), a conditional in the form of
`
if [exp] then [body] else [body] fi [exp]
`,
or a reversible loop in the form of
`
from [exp] do [body] loop [body] until [exp]
`,
where each body is another sequence of one or more SRL blocks.

To ensure reversibility, the conditional ends with an assertion. This assertion **must** evaluate to true if the then-branch was taken, and false otherwise. This makes it clear which path to take when inverting the program.\
\ \ \ \ The loop, on the other hand, *starts* with an assertion. This assertion must hold when entering the loop from the outside, and it must *not* hold when coming from the inside.

The loop works by first evaluating the assertion. If it holds, we continue by executing the first body. If the terminating condition holds afterwards, we are done. If not, we execute the second body and evaluate the assertion once again. This time, if this does *not* hold, we continue and repeat. Thus, we can simulate both do-until- and while loops with this loop construct.

### SRL example
The following program will compute the $n^{th}$ Fibonacci pair reversibly.


```
// Compute the n'th fibonacci pair

int n
int v int w

n ^= 16
w ^= 1
from (v = 0) do
  v += w
  swap v w
  n -= 1
loop .
until (n = 0 || v > w)
```
