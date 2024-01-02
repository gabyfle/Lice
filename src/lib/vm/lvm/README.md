# The Lice bytecode interpreter

Here is the list of instructions available to the bytecode interpreter for the Lice language. This API isn't stable at all and isn't meant to be directly used.

## Set of instructions

| Operation   | Description |
| ----------- | ----------- |
| MOVE A B    | Sets register A equals to the value of register |
| ADD A B C   | Adds values of B and C into A |
| SUB A B C   | Subs C to B and put result into A |
| MUL A B C   | Multiplies B and C and put result into A |
| DIV A B C   | Divides B by C and put result into A |
| NEG A B     | Puts -B inside A
| 
