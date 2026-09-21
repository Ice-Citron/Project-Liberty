

Here is a transcription of the poster, organized by section. Mathematical formulas, logic steps, and equations are formatted using LaTeX, while diagrams have been replaced with descriptive text as requested.

### ISAs
*   **CISC:** (1) dense code, simple compiler. (2) powerful instruction set, variable format.
*   **RISC:** (1) simple instructions, fixed format, optimising compiler. (2) speed, low development cost, adapt to new technology.
*   CISC includes more complex instructions, which reduces the workload of compilers (which translates high-level languages like C into the assembly language). However, this also means that CISC instructions take more time to execute, as they are more complex. Since the compilers are becoming better, RISC is becoming faster and more adaptive to new technologies.

### MIPS
*   representative of modern RISC architectures
*   32 registers `$0..$31` 32 bits each
*   `$0` wired to 0, the others general purpose
*   register-register or load-store architecture
    *   most instructions involve registers only: `add $1, $2, $3 # reg1 = reg2 + reg3`
    *   special memory access instructions: possibly multi cycle `lw $8, Astart($19) # reg8 = M[Astart + reg19]`

### INSTRUCTION TYPES
*   **R-type:** arithmetic, comparison, logical, ...
    *   `add $8, $17, $18 # reg8 = reg17 + reg18`
    *   *[Diagram Description: R-type instruction format showing a 32-bit structure split into opcode (6 bits), rs (5 bits), rt (5 bits), rd (5 bits), shamt (5 bits), and funct (6 bits).]*
*   **immediate (I-type):** memory access, conditional branches, arithmetic involving constants
    *   memory access: `lw $8, Astart($19) # reg8 = M[Astart + reg19]`
    *   *[Diagram Description: I-type instruction format showing a 32-bit structure split into opcode (6 bits), rs (5 bits), rt (5 bits), and immediate constant (16 bits).]*
*   **Jump (J-type):** unconditional jump to instruction in memory
    *   `j 1236 # jump to instruction at address 1236`
    *   *[Diagram Description: J-type instruction format showing a 32-bit structure split into opcode (6 bits) and memory address (26 bits).]*
*   **jal:** jump and link (save address of next instruction in register before jumping)
*   **"jump" instructions can be I-type or R-type**
    *   I-type: `beq`, `bne`, `blez`, `bgtz` `# if reg19 == reg20 goto Label`
    *   R-type: `jr $ra` `# jump to address in register ra`

### CONDITIONALS
*   only 2 conditional branches, `bne` and `beq`
*   need `slt` (set on less than)
    *   `slt $1, $16, $17 # if reg16 < reg17 then reg1 = 1 else reg1 = 0`
*   implement branch to L on `reg16 < reg17` as:
    *   `slt $1, $16, $17 # ... if reg1 != 0 then goto L`
    *   `bne $1, $0, L # (reg0 always 0)`
*   load constant hex `000A000B` to register 5, use load upper/lower immediate (`lui`/`ori`)
    *   `lui $5, 10 # reg5 = 000A0000`
    *   `ori $5, $5, 11 # reg5 = reg5 + 000B`

### PERFORMANCE
*   **CPI:** average clock cycles per instruction
*   $$ \text{number of cycles for program P} = \text{number of instr. for P} \times \text{CPI} $$
*   $$ \text{Execution time for P} = \text{clock cycle time} \times \text{number of cycles for P} $$
*   $$ \text{Execution time for P} = \frac{1}{\text{clock speed}} \times \text{number of cycles for P} $$
*   $$ \text{average exe. time for P1, P2...Pn} = \frac{1}{n} \times (\text{exe. time for P1} + \dots + \text{exe. time for Pn}) $$ *(assume equal workload)*
*   Execution time equation: $$ \text{exe. time} = \text{instr. count} \times \text{CPI} \times \text{cycle time} $$

**Evaluating Performance**
*[Diagram Description: A table evaluating performance methods (actual target workload, full application benchmarks, small kernel benchmarks) comparing their pros and cons. Target workload is representative but specific and not portable. Full application is portable but less representative. Small kernel is easy to use but peak performance is far from typical performance.]*

### AMDAHL'S LAW
*   $$ \text{Execution time after improvement} = \frac{\text{Execution time affected by improvement}}{\text{Amount of improvement}} + \text{Execution time unaffected} $$
*   $$ T_{new} = \frac{\alpha T_{old}}{\beta} + (1-\alpha) T_{old} $$

### MIPS ADDRESSING MODES
*   **Register addressing**, where the operand is a register.
*   **Immediate addressing**, where the operand is a constant within the instruction itself.
*   **Base addressing**, where the operand is at the memory location whose address is the sum of a register and a constant in the instruction.
    *   *[Diagram Description: Visual map showing base addressing. An instruction's register and constant are extracted and summed in memory.]*
*   **PC-relative addressing**, where the branch address is the sum of the PC and a constant in the instruction.
    *   *[Diagram Description: Visual map showing PC-relative addressing using the Program Counter (PC) and a word offset.]*

**Comparing Architectures**
*[Diagram Description: A table comparing temporary storage architectures like Stack (R5000), Accumulator (PDP 8), and Registers/Memory (VAX/MIPS) along with their respective pros and cons.]*

### IMPROVING PERFORMANCE
*   **fast, local store** (e.g. on-chip caches)
*   **concurrent execution of instructions:**
    *   multiple function units: super scalar
    *   "production line" arrangement: pipeline
    *   multiple instruction streams: multi-threading
*   **direct hardware implementation, domain-specific optim.:**
    *   reconfigurable hardware: billions of programmable gates
    *   no fetch / decode, customise at compile time and at run time
*   **other technologies:**
    *   manycore processors: e.g. Graphics Processing Unit (GPU)
    *   quantum computing: based on quantum circuits... qubits

### COMPUTER ARITHMETIC
*   two's complement: signed integer representation
*   e.g. $$ 1011_{2C} = (1 \times -2^3) + (0 \times 2^2) + (1 \times 2^1) + (1 \times 2^0) = -5_{10} $$
*   n-bit: range $$ (-2^{n-1}) \dots (2^{n-1}-1) $$
*   sign extension: $$ 1011_{2C} = 1111011_{2C} $$
*   overflow: $$ A, B > 0, \quad A+B \le 0 $$
    $$ A, B < 0, \quad A+B \ge 0 $$
*   in MIPS: `slt`, `slti` work with two's complement. `sltu`, `sltiu` work with unsigned representation (do not cause exception when overflow).

### LOGICAL OPERATIONS
*   **shift left logical (`sll`)**
    *   `sll $10, $16, 8 # reg10 = reg16 << 8 bits`
    *   reg16: `0.. 0 0000 0000 1101`
    *   reg10: `0.. 0 1101 0000 0000`
    *   *[Diagram Description: R-type instruction bit format visualizing the `sll` operation.]*
*   **shift right logical/arithmetic (`srl`, `sra`)** (sign-extend high order bits)
*   **bitwise:** `or`, `and` (R-type) | `ori`, `andi` (I-type)

### DERIVING ALU ARCHITECTURE & OPERATIONS
*   group components together to form larger repeated unit
*   *[Diagram Description: A series of logic gate schematics demonstrating how an ALU is built from multiplexers (mux), AND/OR gates, and full adders. It shows how these 1-bit cells are connected in series to form a 32-bit ALU.]*
*   $$ d_0d_1 : 00 \text{ AND, } 01 \text{ OR, } 10 \text{ add, } 11 \text{ subtract} $$

**Comparison Operations**
*   `slt`: set on less than. if $$ a < b $$, then 1 else 0
*   if $$ a < b $$, $$ a-b < 0 $$, so MSB of $$ (a-b) $$ is 1 (32 bits)
*   Implementation:
    *   provide additional input to each cell, left of $a$, $b$.
    *   LSB input from MSB ALU's output, other inputs set to 0.
    *   include additional mux in cell for selection.
*   *[Diagram Description: Logic schematic showing zero detection implementation and overflow detection integration in the ALU.]*
*   **Zero Detection:** `beq`, `bne`: test $$ a=b $$ or $$ a-b=0 $$. Include another gate to test if output is zero.

### PERFORMANCE ESTIMATION FOR ALU
*   Speed limited by propagation delay through slowest combinational path (critical path).
*   Critical path is usually the carry path.
*   $$ \text{Max clock rate} = \frac{1}{\text{delay of critical path}} $$
*   Assumes edge-triggered design and ignores flip-flop propagation delay, setup time, clock skew (negligible).

**Faster Addition**
*   **Carry select:** compute both zero-carry-in and one-carry-in. e.g. 8 bits: use three 4-bit ripple carry adders.
*   *[Diagram Description: Schematic showing a carry-select adder using two parallel adders and a multiplexer to speed up computation.]*

### MULTIPLICATION ALGORITHMS
*   $$ \text{multiplicand (mc)} \times \text{multiplier (mp)} = \text{product} $$
    $$ \begin{array}{r@{\quad}l} 0010 & \text{mc} \\ \times 1011 & \text{mp} \\ \hline \dots 0010 & \leftarrow \text{mc shifted 0 bit } \times \text{ bit 0 of mp (1)} \\ \dots 0010 \cdot & \leftarrow \text{mc shifted 1 bit } \times \text{ bit 1 of mp (1)} \\ \dots 0000 \cdot\cdot & \leftarrow \text{mc shifted 2 bits } \times \text{ bit 2 of mp (0)} \\ + 0010 \cdot\cdot\cdot & \leftarrow \text{mc shifted 3 bits } \times \text{ bit 3 of mp (1)} \\ \hline 0010110 & \text{Product} \end{array} $$
*   *[Diagram Description: Hardware diagram for multiplication showing a 32-bit ALU, Multiplicand register, Multiplier register, and a 64-bit Product register.]*
*   *[Diagram Description: A table comparing the step-by-step state of normal multiplication vs Booth's Algorithm over 4 iterations. Text notes there is a typo in the original poster's Booth algorithm 3rd iteration.]*

### DIVISION ALGORITHM
*   $$ dd = (q \times ds) + r $$
*   *[Diagram Description: Block diagram of division hardware showing a 64-bit ALU, Divisor register, Quotient register, and Remainder register, all controlled by a central Control unit.]*
*   *[Diagram Description: Iteration table showing steps 0 through 5 of a binary division process tracking the Remainder, Quotient, and Divisor states.]*
*   *[Diagram Description: Flowchart showing the algorithm: Subtract Divisor from Remainder $\rightarrow$ Test Remainder $\rightarrow$ Shift Quotient $\rightarrow$ Restore Remainder (if needed) $\rightarrow$ Shift Divisor right.]*

### DATAPATHS
**Single Cycle Datapaths**
*   Different data paths for different instruction types (R, I, J)
*   Use multiplexers to combine these datapaths
*   Control unit: activate relevant parts of the combined datapath for a given instruction $\rightarrow$ control signals for multiplexors + ALU
*   *[Diagram Description: Three separate schematic diagrams illustrating the datapath for register-based instructions, memory access instructions, and a combination of both.]*

**Combined Datapath with Controls**
*   *[Diagram Description: A massive, detailed schematic of the complete single-cycle MIPS datapath. It includes Instruction Memory, Registers, the ALU, Data Memory, and comprehensive routing of control signals (RegDst, Branch, MemRead, MemtoReg, ALUOp, MemWrite, ALUSrc, RegWrite).]*
*   **ALU:** ALUOp
*   **control 4 mux:** RegDst, ALUSrc, MemtoReg, PCSrc
*   **storage:** Register Write, Data Memory Read/Write

**Multi Cycle Datapaths**
*   Each cycle covers only one part of execution
*   Multiple cycles for each instruction
*   ALU used differently in different cycles
*   RTL assignments:
    *   cycle 3 `lw`: $$ \text{MDR} = \text{M[ALUOut]} $$
    *   cycle 4 `lw`: $$ \text{Reg[IR_{20-16}]} = \text{MDR} $$
*   *[Diagram Description: High-level datapath showing temporary state registers like IR (Instruction Register) and MDR (Memory Data Register) added between stages.]*

**Register Transfer Level (RTL) for Load Instruction**
*   cycle 1: $$ \text{IR} = \text{M[PC]}, \text{PC} = \text{PC} + 4 $$
*   cycle 2: $$ \text{A} = \text{Reg[rs]}, \text{B} = \text{Reg[rt]} $$
*   cycle 3: $$ \text{ALUOut} = \text{A} + \text{sign-ext(imm)} $$
*   cycle 4: $$ \text{MDR} = \text{M[ALUOut]} $$
*   cycle 5: $$ \text{Reg[rt]} = \text{MDR} $$
*   *[Diagram Description: Detailed schematic of the multi-cycle datapath indicating exactly where intermediate registers (IR, MDR, A, B, ALUOut) sit to hold data between clock cycles.]*

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