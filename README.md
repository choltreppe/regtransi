# RegTransI

RegTransI is a register transfer command interpreter (including RAM simulation and jumps), that I wrote for helping me with a university assignment about programming a CU of a CPU. Maybe its usefull for some other students that have simmilar assignments.

## Installation
### build yourself
1. install nim, if you haven't: https://nim-lang.org/install.html
2. clone the repo:
```
git clone https://github.com/choltreppe/regtransi
```
3. build with nimble:
```
cd regtransi
nimble install
```

## Usage
### running a rt program
to run a rt program use the `regtransi run` command:<br>
(the syntax of programs is explained below)
```
regtransi run -p your_program.rt
```
### using RAM
By default the RAM is 0 initialized.<br>
To set a different initial RAM, use the `-i` parameter:
```
regtransi run -p your_program.rt -i inital_ram
```
To safe the RAM content at the end, use the `-o` parameter:
```
regtransi run -p your_program.rt -i inital_ram -o result
```

## Examples
Check out the example programs in `examples/`.

## Syntax
Comments start with a `#`:
```# this is a comment```

RT-commands work as you expect:
- commands that are executed parallel are seperated with `:`
- commands that are executed sequentially are seperated with `;`
- the lhs of a transfer is always a register or a MEM access
- the rhs is either just a register/mem/constant or a binary/unary expression
  - unary: `~`
  - binary: `+`, `-`, `*`, `/`, `%`, `&`, `|`, `^`, `<<`, `>>`
```
REGB <- REGA : REGC <- REGA;
REGB <- REGB + 6
```

### MEM access
To access memmory use `[]`:
```
MDR <- [MAR]  # read
[MAR] <- MDR  # write
```

### Jumps
To use jumps you need to mark positions in code with `labels`<br>
To define a label add a line with only `@` followed by the `label`s name
```
A <- 0
@some_label
B <- A
```
Make unconditional jump:
```
goto some_label
```
Make conditional jump:
```
goto if A == B then some_label
```
or
```
goto if A == B then some_label else some_other_label
```
`A` and `B` may be a register, mem-access, constant or expression.<br>
You can use the following comparisons: `==`, `!=`, `<`, `<=`, `>`, `>=`