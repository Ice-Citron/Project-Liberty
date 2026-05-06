

Here is a transcription of the content from the provided cheat sheet. I have used Markdown to organize the sections, LaTeX for the mathematical formulas and formal symbols, and provided brief descriptions for the complex diagrams as requested.

### Left Column: Control, State Machines, & Exceptions

**STATE DIAGRAM**
> *Diagram Description:* A multi-cycle state machine diagram showing the sequence of operations for instruction execution, transitioning through fetch, decode, execute, memory access, and write-back states.

**STATE DIAGRAM W/ CONTROL SIGNALS**
> *Diagram Description:* Similar to the state diagram above, but this version annotates each transition and state with specific hardware control signals (e.g., `MemRead`, `ALUSrcA = 0`, `PCWrite`).

**FINITE STATE MACHINE**
> *Diagram Description:* A simple block diagram of a finite state machine. It shows inputs feeding into a "combinational logic" block. The logic block outputs to both the external outputs and a "state reg" (current state to next state), which loops back into the combinational logic.

**ROM: Read Only Memory**
> *Diagram Description:* A basic rectangular block representing a ROM chip, parameterized by size: $2^n \times d$. It takes an $n$-bit address and outputs $d$-bit data.

**MICROPROGRAM TABLE**

| State | Label | ALU Control | SRC 1 | SRC 2 | Memory | Reg. Control | PC write | Sequencing |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| 0 | Fetch | Add | PC | 4 | ReadPC | Read | ALU | Seq |
| 1 | | Add | PC | ExtShft | | Read | | Dispatch 1 |
| 2 | Mem1 | Add | A | Extend | | | | Dispatch 2 |
| 3 | LW2 | | | | ReadALU | WriteMDR | | Fetch |
| 4 | SW2 | | | | WriteALU | | | Fetch |
| 5 | Rformat1 | Rformat | A | B | | WriteALU | | Seq |
| 6 | BEQ1 | beq | A | B | | | ALUout-cond | Fetch |
| 7 | JUMP1 | | | | | | Jump addr | Fetch |

**MICROSEQUENCERS**
> *Diagram Description:* A block diagram showing the flow of microcode execution. A 4-bit microprogram counter ($\mu\text{PC}$) feeds into a ROM containing combinational control logic. The output drives both datapath control and the address select logic (which determines whether to sequence +1 or branch to a new state).

**Dispatch ROM Tables**
*   **Dispatch ROM 1:** Maps Opcode values (like `Rformat`, `jmp`, `beq`, `lw`, `sw`) to state values.
*   **Dispatch ROM 2:** Maps `lw` and `sw` opcodes to their respective secondary states.

---

### Second Column: Microcoding, Memory, & Datatypes

**MICROCODING**
*   **Horizontal:** 1 bit of control logic for each control output bit (e.g., $n = m = 16$)
    *   exploits parallelism of operations in data path
    *   minimal control overhead
    *   requires large ROM
*   **Vertical:** encode the control logic so that $n < m$ then use a decoder to produce the data path control signals
    *   one operation at a time; easier to understand and use
    *   slower due to decoding delay and sequentialization of instructions

**EXCEPTIONS**
*   emergencies, requiring a change in normal flow of instruction execution
*   **exception:** unexpected event from within processor (e.g., arithmetic overflow)
*   **interrupt:** unexpected event from external environment (e.g., input/output request)

**HANDLING EXCEPTIONS**
*   add address of instruction causing exception to EPC (Exception Program Counter)
*   new control signal `IntCause` from control unit to cause register
*   e.g. `IntCause = 0` for unknown opcode, `= 1` for arithmetic overflow
*   **restart address:** address of instruction causing exception and is saved in exception PC (EPC)
*   transfer control to operating system by executing instruction at exception address in memory
*   code at exception address can find out what happens by inspecting cause register

> *Diagram Description:* A large, complex "Multi-cycle datapath with exception" schematic. It illustrates the complete CPU hardware, including ALU, registers, memory blocks, PC, exception PC (EPC), Cause register, and the multiplexers/control lines connecting them. Below it are small state diagram additions showing how states 10 and 11 handle undefined instructions and overflow.

**C DATATYPES**

| C declaration | Intel data type | Assembly code suffix | Size (bytes) |
| :--- | :--- | :--- | :--- |
| `char` | Byte | `b` | 1 |
| `short` | Word | `w` | 2 |
| `int` | Double word | `l` | 4 |
| `long int` | Quad word | `q` | 8 |
| `char*` | Quad word | `q` | 8 |
| `float` | Single precision | `s` | 4 |
| `double` | Double precision | `l` | 8 |
| `long double`| Extended precision | `t` | 10/16 |

**MEMORY ADDRESSING MODES**
*   **Imm:** `Mem[ Imm ]`
*   **[r_a]:** `Mem[ R[ r_a ] ]`
*   **Imm[r_a]:** `Mem[ R[ r_a ] + Imm ]`
*   **Imm[r_b, r_i, s]:** `Mem[ R[ r_b ] + R[ r_i ] \times s + Imm ]`
    *   s can only be 1, 2, 4, or 8
*   **Imm[r_b]:** `Mem[ R[ r_b ] + Imm ]`
*   **[r_b, r_i]:** `Mem[ R[ r_b ] + R[ r_i ] ]`
*   **Imm[, r_i, s]:** `Mem[ R[ r_i ] \times s + Imm ]`

**OPERAND COMBOS FOR MOVL**
Describes valid combinations of Source (Immediate, Register, Memory) and Destination (Register, Memory) for the `movl` instruction, including C analogs (e.g., `temp = 0x4`, `*p = temp`). Note: Memory to Memory is not allowed.

---

### Third Column: Registers, Condition Codes, & Loops

**INTEGER REGISTERS**
> *Diagram Description:* A visual chart of the x86-64 integer registers, illustrating how 64-bit registers (like `%rax`) are subdivided into 32-bit (`%eax`), 16-bit (`%ax`), and 8-bit (`%al`) chunks. It labels their standard usage conventions (e.g., `%rax` for return value, `%rdi`/`%rsi` for arguments, `%rsp` for stack pointer).
*   **Caller-saved:** Saved by caller before function call (if needed). May be overwritten by callee. Used for temporary values.
*   **Callee-saved:** Saved/restored by callee if it uses them. Preserved across function calls. Used for long-lived values.

**CONDITION CODES**
Implicitly set (think of it as a side effect) by arithmetic operations (not by `lea`)
Example: `addl/addq Src, Dest` $\leftrightarrow t = a + b$
*   `CF` set if carry out from most significant bit (unsigned overflow)
*   `ZF` set if $t == 0$
*   `SF` set if $t < 0$ (as signed)
*   `OF` set if two's complement (signed) overflow: `(a>0 && b>0 && t<0) || (a<0 && b<0 && t>=0)`

**Explicit setting by a compare instruction**
`cmpl/cmpq Src2, Src1`
Example: `cmpl b, a` like computing $a - b$ without setting destination
*   `CF` set if carry out from most significant bit (used for unsigned comparisons)
*   `ZF` set if $a == b$
*   `SF` set if $(a - b) < 0$ (as signed)
*   `OF` set if two's complement (signed) overflow: `(a>0 && b<0 && (a-b)<0) || (a<0 && b>0 && (a-b)>0)`

**Explicit setting by a test instruction**
`testl/testq Src2, Src1`
Example: `testl b, a` like computing $a \ \& \ b$ without setting destination
*   Sets condition codes based on value of $Src1 \ \& \ Src2$
*   Useful to have one of the operands be a mask
*   `ZF` set when $a \ \& \ b == 0$
*   `SF` set when $a \ \& \ b < 0$
*   `testl %eax, %eax` sets SF and ZF, check if eax is $\le 0$.

*   `set dest`: set low-order byte to 0 or 1 based on CCs.
*   `cmov src, dest`: move only if condition holds.

*(Includes a table detailing instructions like `jmp`, `je`, `jne`, `js`, `jns`, `jg`, `jge`, `jl`, `jle`, `ja`, `jb` and their corresponding conditions based on flags).*

**TRANSLATING LOOPS**
*   **"Jump-to-middle" translation:** Used with `-Og`. Recent technique for GCC. Translates a `while` loop into an unconditional jump to the loop's test condition, which then branches back to the loop body.
*   **"guarded do" (do-while) conversion:** Used with `-O1`. Translates a `while` loop by using an initial `if` statement to skip the loop entirely if the condition is false, followed by a `do-while` style loop.

---

### Fourth Column: Switches, Stack, Caches & Locality

**JUMP TABLE (for switches)**
Translates a C `switch` statement using an array of branch targets.
Assembly uses: `jmp *.L4(, %rdi, 8)` (Indirect jump).
`.rodata` section contains the Jump Table `.quad` values mapping to code labels.

**STACK PROCEDURE**
*   **Procedure call:** `call label`
    *   push return address on stack
    *   jmp to label
*   **Return address:** Address of instruction beyond `call`.
*   **Procedure return:** `ret`
    *   pop address from stack
    *   jmp to address
*   **Array Elements:**
    *   `A[i][j]` is element of type T, which requires K bytes
    *   $\text{Address} = A + i \cdot (C \cdot K) + j \cdot K = A + (i \cdot C + j) \cdot K$

**LOCALITY**
*   **Temporal locality:** Recently referenced items are likely to be referenced again in the near future.
*   **Spatial locality:** Items with nearby addresses tend to be referenced close together in time.

**CACHE PERFORMANCE**
*   **Miss rate:** Fraction of memory references not found in cache (misses / accesses) or ($1 - \text{hit rate}$)
*   **Hit time:** Time to deliver a block in the cache to the processor - Includes time to determine whether the block is in the cache
*   **Miss penalty:** Additional time required because of a miss
*   **Average Memory Access Time (AMAT):** average time to access memory considering both hits and misses
    $$AMAT = \text{Hit time} + \text{Miss rate} \times \text{Miss penalty}$$

**CACHES AND ASSOCIATIVITY**
> *Diagram Description:* A visual comparison of cache associativity types showing how blocks are mapped. It contrasts "1 way, 8 sets" (Direct mapped), "2 way, 4 sets", "4 way, 2 sets", and "8 way, 1 sets" (Fully associative).

**Cache Read**
> *Diagram Description:* A diagram detailing the anatomy of a cache read operation. It shows how a memory address is split into `Tag`, `Set Index`, and `Block Offset`. It outlines the 4 steps: 1) Locate set via index, 2) Check if any line in the set has a matching tag, 3) If yes and the valid bit is 1, it's a Hit, 4) Locate data within the block using the offset.

**Cache Misses**
*   **Compulsory (cold) miss:** Occurs on first access to a block.
*   **Conflict miss:** Occurs when the cache is large enough, but multiple data objects all map to the same slot. E.g., referencing blocks 0, 8, 0, 8... could miss every time. Direct-mapped caches have more conflict misses than N-way set-associative.
*   **Capacity miss:** Occurs when the set of active cache blocks (the working set) is larger than the cache. Note: Fully-associative only has Compulsory and Capacity misses.

**WHAT TO DO ON A WRITE-HIT**
*   **Write-through:** Write immediately to memory and all caches in between. Memory is always consistent with the cache copy. Slow; what if the same value (or line!) is written several times.
*   **Write-back:** Defer write to memory until line is evicted (replaced). Need a dirty bit (line is different from memory). Higher performance (but more complex).

**WHAT TO DO ON A WRITE-MISS**
*   **Write-allocate (load into cache, update line in cache):** Good if more writes to the location follow. More complex to implement. May evict an existing value. Common with write-back caches.
*   **No-write-allocate (writes immediately to memory):** Simpler to implement. Slower code (bad if value consistently re-read). Seen with write-through caches.

> *Diagram Description (Far Right Edge):* A vertical diagram showing how an A-bit memory address maps down the hierarchy. It illustrates that the `Offset` bits select the byte from the block, the `Index` bits select the set (which shrinks as associativity increases), and the `Tag` bits are used for comparison (which grows as associativity increases).


---


# Page 2



Here is a transcription of the content from the provided cheat sheet. I have used Markdown to organize the sections, LaTeX for the mathematical formulas and formal symbols, and provided brief descriptions for the complex diagrams as requested.

### Left Column: Control, State Machines, & Exceptions

**STATE DIAGRAM**
> *Diagram Description:* A multi-cycle state machine diagram showing the sequence of operations for instruction execution, transitioning through fetch, decode, execute, memory access, and write-back states.

**STATE DIAGRAM W/ CONTROL SIGNALS**
> *Diagram Description:* Similar to the state diagram above, but this version annotates each transition and state with specific hardware control signals (e.g., `MemRead`, `ALUSrcA = 0`, `PCWrite`).

**FINITE STATE MACHINE**
> *Diagram Description:* A simple block diagram of a finite state machine. It shows inputs feeding into a "combinational logic" block. The logic block outputs to both the external outputs and a "state reg" (current state to next state), which loops back into the combinational logic.

**ROM: Read Only Memory**
> *Diagram Description:* A basic rectangular block representing a ROM chip, parameterized by size: $2^n \times d$. It takes an $n$-bit address and outputs $d$-bit data.

**MICROPROGRAM TABLE**

| State | Label | ALU Control | SRC 1 | SRC 2 | Memory | Reg. Control | PC write | Sequencing |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| 0 | Fetch | Add | PC | 4 | ReadPC | Read | ALU | Seq |
| 1 | | Add | PC | ExtShft | | Read | | Dispatch 1 |
| 2 | Mem1 | Add | A | Extend | | | | Dispatch 2 |
| 3 | LW2 | | | | ReadALU | WriteMDR | | Fetch |
| 4 | SW2 | | | | WriteALU | | | Fetch |
| 5 | Rformat1 | Rformat | A | B | | WriteALU | | Seq |
| 6 | BEQ1 | beq | A | B | | | ALUout-cond | Fetch |
| 7 | JUMP1 | | | | | | Jump addr | Fetch |

**MICROSEQUENCERS**
> *Diagram Description:* A block diagram showing the flow of microcode execution. A 4-bit microprogram counter ($\mu\text{PC}$) feeds into a ROM containing combinational control logic. The output drives both datapath control and the address select logic (which determines whether to sequence +1 or branch to a new state).

**Dispatch ROM Tables**
*   **Dispatch ROM 1:** Maps Opcode values (like `Rformat`, `jmp`, `beq`, `lw`, `sw`) to state values.
*   **Dispatch ROM 2:** Maps `lw` and `sw` opcodes to their respective secondary states.

---

### Second Column: Microcoding, Memory, & Datatypes

**MICROCODING**
*   **Horizontal:** 1 bit of control logic for each control output bit (e.g., $n = m = 16$)
    *   exploits parallelism of operations in data path
    *   minimal control overhead
    *   requires large ROM
*   **Vertical:** encode the control logic so that $n < m$ then use a decoder to produce the data path control signals
    *   one operation at a time; easier to understand and use
    *   slower due to decoding delay and sequentialization of instructions

**EXCEPTIONS**
*   emergencies, requiring a change in normal flow of instruction execution
*   **exception:** unexpected event from within processor (e.g., arithmetic overflow)
*   **interrupt:** unexpected event from external environment (e.g., input/output request)

**HANDLING EXCEPTIONS**
*   add address of instruction causing exception to EPC (Exception Program Counter)
*   new control signal `IntCause` from control unit to cause register
*   e.g. `IntCause = 0` for unknown opcode, `= 1` for arithmetic overflow
*   **restart address:** address of instruction causing exception and is saved in exception PC (EPC)
*   transfer control to operating system by executing instruction at exception address in memory
*   code at exception address can find out what happens by inspecting cause register

> *Diagram Description:* A large, complex "Multi-cycle datapath with exception" schematic. It illustrates the complete CPU hardware, including ALU, registers, memory blocks, PC, exception PC (EPC), Cause register, and the multiplexers/control lines connecting them. Below it are small state diagram additions showing how states 10 and 11 handle undefined instructions and overflow.

**C DATATYPES**

| C declaration | Intel data type | Assembly code suffix | Size (bytes) |
| :--- | :--- | :--- | :--- |
| `char` | Byte | `b` | 1 |
| `short` | Word | `w` | 2 |
| `int` | Double word | `l` | 4 |
| `long int` | Quad word | `q` | 8 |
| `char*` | Quad word | `q` | 8 |
| `float` | Single precision | `s` | 4 |
| `double` | Double precision | `l` | 8 |
| `long double`| Extended precision | `t` | 10/16 |

**MEMORY ADDRESSING MODES**
*   **Imm:** `Mem[ Imm ]`
*   **[r_a]:** `Mem[ R[ r_a ] ]`
*   **Imm[r_a]:** `Mem[ R[ r_a ] + Imm ]`
*   **Imm[r_b, r_i, s]:** `Mem[ R[ r_b ] + R[ r_i ] \times s + Imm ]`
    *   s can only be 1, 2, 4, or 8
*   **Imm[r_b]:** `Mem[ R[ r_b ] + Imm ]`
*   **[r_b, r_i]:** `Mem[ R[ r_b ] + R[ r_i ] ]`
*   **Imm[, r_i, s]:** `Mem[ R[ r_i ] \times s + Imm ]`

**OPERAND COMBOS FOR MOVL**
Describes valid combinations of Source (Immediate, Register, Memory) and Destination (Register, Memory) for the `movl` instruction, including C analogs (e.g., `temp = 0x4`, `*p = temp`). Note: Memory to Memory is not allowed.

---

### Third Column: Registers, Condition Codes, & Loops

**INTEGER REGISTERS**
> *Diagram Description:* A visual chart of the x86-64 integer registers, illustrating how 64-bit registers (like `%rax`) are subdivided into 32-bit (`%eax`), 16-bit (`%ax`), and 8-bit (`%al`) chunks. It labels their standard usage conventions (e.g., `%rax` for return value, `%rdi`/`%rsi` for arguments, `%rsp` for stack pointer).
*   **Caller-saved:** Saved by caller before function call (if needed). May be overwritten by callee. Used for temporary values.
*   **Callee-saved:** Saved/restored by callee if it uses them. Preserved across function calls. Used for long-lived values.

**CONDITION CODES**
Implicitly set (think of it as a side effect) by arithmetic operations (not by `lea`)
Example: `addl/addq Src, Dest` $\leftrightarrow t = a + b$
*   `CF` set if carry out from most significant bit (unsigned overflow)
*   `ZF` set if $t == 0$
*   `SF` set if $t < 0$ (as signed)
*   `OF` set if two's complement (signed) overflow: `(a>0 && b>0 && t<0) || (a<0 && b<0 && t>=0)`

**Explicit setting by a compare instruction**
`cmpl/cmpq Src2, Src1`
Example: `cmpl b, a` like computing $a - b$ without setting destination
*   `CF` set if carry out from most significant bit (used for unsigned comparisons)
*   `ZF` set if $a == b$
*   `SF` set if $(a - b) < 0$ (as signed)
*   `OF` set if two's complement (signed) overflow: `(a>0 && b<0 && (a-b)<0) || (a<0 && b>0 && (a-b)>0)`

**Explicit setting by a test instruction**
`testl/testq Src2, Src1`
Example: `testl b, a` like computing $a \ \& \ b$ without setting destination
*   Sets condition codes based on value of $Src1 \ \& \ Src2$
*   Useful to have one of the operands be a mask
*   `ZF` set when $a \ \& \ b == 0$
*   `SF` set when $a \ \& \ b < 0$
*   `testl %eax, %eax` sets SF and ZF, check if eax is $\le 0$.

*   `set dest`: set low-order byte to 0 or 1 based on CCs.
*   `cmov src, dest`: move only if condition holds.

*(Includes a table detailing instructions like `jmp`, `je`, `jne`, `js`, `jns`, `jg`, `jge`, `jl`, `jle`, `ja`, `jb` and their corresponding conditions based on flags).*

**TRANSLATING LOOPS**
*   **"Jump-to-middle" translation:** Used with `-Og`. Recent technique for GCC. Translates a `while` loop into an unconditional jump to the loop's test condition, which then branches back to the loop body.
*   **"guarded do" (do-while) conversion:** Used with `-O1`. Translates a `while` loop by using an initial `if` statement to skip the loop entirely if the condition is false, followed by a `do-while` style loop.

---

### Fourth Column: Switches, Stack, Caches & Locality

**JUMP TABLE (for switches)**
Translates a C `switch` statement using an array of branch targets.
Assembly uses: `jmp *.L4(, %rdi, 8)` (Indirect jump).
`.rodata` section contains the Jump Table `.quad` values mapping to code labels.

**STACK PROCEDURE**
*   **Procedure call:** `call label`
    *   push return address on stack
    *   jmp to label
*   **Return address:** Address of instruction beyond `call`.
*   **Procedure return:** `ret`
    *   pop address from stack
    *   jmp to address
*   **Array Elements:**
    *   `A[i][j]` is element of type T, which requires K bytes
    *   $\text{Address} = A + i \cdot (C \cdot K) + j \cdot K = A + (i \cdot C + j) \cdot K$

**LOCALITY**
*   **Temporal locality:** Recently referenced items are likely to be referenced again in the near future.
*   **Spatial locality:** Items with nearby addresses tend to be referenced close together in time.

**CACHE PERFORMANCE**
*   **Miss rate:** Fraction of memory references not found in cache (misses / accesses) or ($1 - \text{hit rate}$)
*   **Hit time:** Time to deliver a block in the cache to the processor - Includes time to determine whether the block is in the cache
*   **Miss penalty:** Additional time required because of a miss
*   **Average Memory Access Time (AMAT):** average time to access memory considering both hits and misses
    $$AMAT = \text{Hit time} + \text{Miss rate} \times \text{Miss penalty}$$

**CACHES AND ASSOCIATIVITY**
> *Diagram Description:* A visual comparison of cache associativity types showing how blocks are mapped. It contrasts "1 way, 8 sets" (Direct mapped), "2 way, 4 sets", "4 way, 2 sets", and "8 way, 1 sets" (Fully associative).

**Cache Read**
> *Diagram Description:* A diagram detailing the anatomy of a cache read operation. It shows how a memory address is split into `Tag`, `Set Index`, and `Block Offset`. It outlines the 4 steps: 1) Locate set via index, 2) Check if any line in the set has a matching tag, 3) If yes and the valid bit is 1, it's a Hit, 4) Locate data within the block using the offset.

**Cache Misses**
*   **Compulsory (cold) miss:** Occurs on first access to a block.
*   **Conflict miss:** Occurs when the cache is large enough, but multiple data objects all map to the same slot. E.g., referencing blocks 0, 8, 0, 8... could miss every time. Direct-mapped caches have more conflict misses than N-way set-associative.
*   **Capacity miss:** Occurs when the set of active cache blocks (the working set) is larger than the cache. Note: Fully-associative only has Compulsory and Capacity misses.

**WHAT TO DO ON A WRITE-HIT**
*   **Write-through:** Write immediately to memory and all caches in between. Memory is always consistent with the cache copy. Slow; what if the same value (or line!) is written several times.
*   **Write-back:** Defer write to memory until line is evicted (replaced). Need a dirty bit (line is different from memory). Higher performance (but more complex).

**WHAT TO DO ON A WRITE-MISS**
*   **Write-allocate (load into cache, update line in cache):** Good if more writes to the location follow. More complex to implement. May evict an existing value. Common with write-back caches.
*   **No-write-allocate (writes immediately to memory):** Simpler to implement. Slower code (bad if value consistently re-read). Seen with write-through caches.

> *Diagram Description (Far Right Edge):* A vertical diagram showing how an A-bit memory address maps down the hierarchy. It illustrates that the `Offset` bits select the byte from the block, the `Index` bits select the set (which shrinks as associativity increases), and the `Tag` bits are used for comparison (which grows as associativity increases).