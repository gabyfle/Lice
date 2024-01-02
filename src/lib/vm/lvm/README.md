# The Lice bytecode interpreter

Here is the list of instructions available to the bytecode interpreter for the Lice language. This API isn't stable at all and isn't meant to be directly used.

## Set of instructions

### Memory
| Operation   | Description |
| ----------- | ----------- |
| `LOAD A B` | Loads the value from memory location B into register A. |
| `STORE A B` | Stores the value from register B into memory location A. |
| `MOVE A B` | Copies the value from register B into register A. |

### Stack Operators

| Operation   | Description |
| ----------- | ----------- |
| `PUSH A` | Pushes the value in register A onto the stack. |
| `POP A` | Pops a value from the stack into register A. |

### Arithmetics
| Operation   | Description |
| ----------- | ----------- |
| `ADD A B C`   | Adds values of B and C into A |
| `SUB A B C`   | Subs C to B and put result into A |
| `MUL A B C`   | Multiplies B and C and put result into A |
| `DIV A B C`   | Divides B by C and put result into A |
| `MOD A B C`   | Computes the remainder of dividing the value in register B by the value in register C, and stores the result in register A. |
| `NEG A B`     | Puts -B inside A

### Comparison

| Operation   | Description |
| ----------- | ----------- |
| `LT A B C`  | Checks if the value in register B is less than the value in register C, and stores the result in register A. |
| `GT A B C` | Checks if the value in register B is greater than the value in register C, and stores the result in register A. |
| `LE A B C` | Checks if the value in register B is less than or equal to the value in register C, and stores the result in register A. |
| `GE A B C` | Checks if the value in register B is greater than or equal to the value in register C, and stores the result in register A. |
| `EQ A B C` | Checks if the value in register B is equal to the value in register C, and stores the result in register A. |
| `NE A B C` | Checks if the value in register B is not equal to the value in register C, and stores the result in register A. |
