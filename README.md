# Project Liberty

My personal archive of technical study and programming practice.

> Slow is fast.

This repository records how I move from an unfamiliar concept to an independent solution.

I type explanations and source code manually to control my pace. I use questions and repeated practice to develop my understanding. I also ask AI tools to make interactive lessons that connect equations to visual examples.

[Interactive lessons](#interactive-lessons) · [QSCHA](#qscha) · [Subjects](#subjects-in-the-archive) · [Codebase studies](#codebase-studies) · [Repository guide](#repository-guide)

## Why this repository exists

When I read technical material too quickly, I can lose the connection between each step. I type the explanation manually to slow down. This gives me time to examine the notation and identify gaps in my understanding.

An unfamiliar term often leads to another question. I study that question in a separate conversation, then return to the original topic. This can involve several explanations before the full concept becomes clear.

The repository started with manual transcription of existing codebases. It now also contains:

- Technical notes and worked examples.
- Programming exercises and solution attempts.
- Mathematics and proof practice.
- Interactive HTML lessons.
- Research papers and related notes.
- Instructions for my AI study tools.

The goal is to understand the material well enough to use it independently.

## How I study

1. Choose a concept or practical problem.
2. Ask for an intuitive explanation.
3. Examine the technical definitions and notation.
4. Type the explanation or example manually.
5. Study unfamiliar prerequisites.
6. Solve exercises with QSCHA.
7. Repeat similar exercises with fewer hints.
8. Apply the concept to a larger problem.

I use visual examples when an equation alone does not give me a clear mental picture. For code, I also examine how each function connects to the rest of the program.

## QSCHA

QSCHA means **Questions with Syntactical and Conceptual Hints, then Answers**.

A programming exercise can present two separate difficulties:

- I do not know the syntax or library interface.
- I do not understand the algorithm or mathematical steps.

QSCHA gives each difficulty its own type of hint.

| Part | Purpose |
|---|---|
| Question | State the problem and the required result. |
| Syntactical hint | Give the exact functions, arguments, and return types. Include array shapes when necessary. |
| Conceptual hint | Explain the logic or mathematical steps. |
| Answer | Give a reference solution after an attempt. |

For mathematics, syntactical hints give the notation and relevant definitions. Conceptual hints give the proof strategy or calculation steps.

The hints become less detailed as I improve:

| Level | Help available |
|---|---|
| 1. Full guidance | Ordered steps and a complete list of syntax. |
| 2. Unordered syntax | Ordered steps and an unordered list of syntax. |
| 3. Function names | Ordered steps and function names without arguments. |
| 4. Logic only | Conceptual steps without syntax. |
| 5. Independent attempt | A short problem statement without a detailed solution plan. |

Answers remain separate from the questions. After an attempt, I compare the solutions and examine the differences.

I repeat related questions until I can solve them with less help.

The [QSCHA method document](Copied/Claudex/standard-technical-codex-skills/skills/standard-technical-qscha/references/qscha-engine.md) contains the detailed format.

## Interactive lessons

I ask AI tools to make visual explanations for concepts that I want to understand.

These HTML lessons let me change parameters and compare the result with my prediction. The diagrams connect geometric behavior to the corresponding equations.

| Lesson | Topics |
|---|---|
| [Essence of Similarity Transforms](Copied/Agent_Artifacts/Essence-of-Similarity-Transforms/index.html) | Matrix columns, coordinate transforms, uniform scale, and the image transforms used in GigaPose. |
| [Essence of Rotations and Rigid Motions](Copied/Agent_Artifacts/Essence-of-SO3-SE3-Geodesic-Error/index.html) | SO(3), axis-angle rotation, exponential and logarithmic maps, SE(3), and pose error. |
| [Essence of BOP Pose Errors](Copied/Agent_Artifacts/Essence-of-BOP-Pose-Errors/index.html) | MSSD, MSPD, VSD, object symmetry, visibility, and pose evaluation. |

The chapters include:

- Controls for the visual examples.
- Questions to answer before each experiment.
- Mathematical explanations.
- Comparisons between related concepts.

For example, the similarity-transform lessons examine two different uses of the word “similar.” One concerns a change of basis. The other concerns a geometric transformation with uniform scale.

The BOP lessons compare how different metrics measure the same pose error.

The links above open HTML source files on GitHub. To use the lessons, download the repository and open the relevant `index.html` in a web browser.

## Subjects in the archive

### Computer science and mathematics

| Subject | Examples in the notes |
|---|---|
| [Computer systems](Copied/40001-Computer-Systems/) | Boolean algebra, Karnaugh maps, latches, flip-flops, finite-state machines, and processor datapaths. |
| [Computer architecture](Copied/40005-Computer-Architecture/) | RISC-V instruction formats, ALU design, processor control, pipeline hazards, x86-64 assembly, and caches. |
| [Graphs and algorithms](Copied/40008-Graphs-%26-Algorithms/) | Graph isomorphism, BFS, DFS, shortest paths, minimum spanning trees, dynamic programming, and NP-completeness. |
| [Databases](Copied/40007-Databases/) | Relational algebra, functional dependencies, normalization, transaction schedules, concurrency control, and Datalog. |
| [SQL practice](Copied/SQL-Practice/) | Joins, keys, NULL semantics, aggregates, window functions, and entity–relationship models. |
| [Haskell](Copied/40009-Haskell-Practice/) | Recursion, algebraic data types, folds, lazy evaluation, typeclasses, parser combinators, and monads. |
| [Calculus](Copied/40016-Calculus/) | Limits, continuity, Riemann sums, convergence, Taylor series, and metric spaces. |
| [Linear algebra](Copied/40017-Linear-Algebra/) | Gaussian elimination, vector spaces, rank and nullity, eigenvalues, changes of basis, and orthogonal projection. |
| [Discrete mathematics](Copied/40018A-Discrete_Maths/) | Relations, equivalence classes, partial orders, functions, cardinality, and countability. |
| [Logic](Copied/40018B-Logic/) | Propositional logic, first-order logic, natural deduction, quantifiers, and normal forms. |
| [Program reasoning](Copied/40018C-Reasoning/) | Mathematical induction, structural induction, proof obligations, loop invariants, and termination. |

### Robotics, machine learning, and software tools

| Subject | Examples in the notes |
|---|---|
| [Robot simulation and control](Copied/Nvidia-Issac-Lab/) | MuJoCo C API, position control, finite-state controllers, simulation stability, and insertion-score calculations. |
| [Robot geometry and perception](Copied/Nvidia-Issac-Lab/) | Coordinate frames, Jacobians, singularities, quaternions, SLERP, and ChArUco camera calibration. |
| [Machine learning and numerical tools](Copied/Nvidia-Issac-Lab/) | NumPy arrays, transformer attention, KV caches, low-rank methods, flow matching, and robot foundation models. |
| [Robotics platforms](Copied/Nvidia-Webinars/) | Isaac Sim, Isaac Lab, synthetic data, robot deployment, and Jetson hardware. |
| [AI-agent architecture](Copied/Project%20Altiera/) | OpenClaw architecture, tool interfaces, plugins, memory search, WebSockets, and Android build workflows. |
| [Additional study material](Copied/Imperial%20Miscellaneous/) | Robot software interview practice and presentation diagrams about AI distillation and its wider effects. |

The dated files preserve questions, partial attempts, and later explanations. They show the steps of the study process.

## Selected notes

These files give concrete examples of that process:

- [From vectors to an insertion score](Copied/Nvidia-Issac-Lab/June-14.ipynb)  
  Notes connect vector projection and lateral error to a robot insertion benchmark.

- [Camera calibration and ChArUco boards](Copied/Nvidia-Issac-Lab/July-16/July-16.ipynb)  
  Notes examine corner identifiers and the structure of calibration code.

- [Parser combinators in Haskell](Copied/40009-Haskell-Practice/Dec-30_v3.hs)  
  Practice combines small parsers through `Functor`, `Applicative`, and `Alternative`.

- [Loop invariants and proof obligations](Copied/40018C-Reasoning/Apr-18.ipynb)  
  Notes connect program statements to the conditions necessary for a correctness proof.

- [Graph structure through visual examples](Copied/40008-Graphs-%26-Algorithms/23-Jan.ipynb)  
  Notebook examples use NetworkX and Matplotlib to examine graph structure and symmetry.

## Codebase studies

Manual source-code transcription remains part of the method.

I use it to examine how an existing implementation represents data and divides responsibilities between functions. These folders contain study copies of code from other projects.

| Codebase | Material in this archive |
|---|---|
| [llama.cpp and GGML](Copied/llama.cpp/) | Public interfaces, tensor operations, architecture definitions, batches, and LoRA adapters. |
| [AlphaZero Chess](Copied/AlphaZero_Chess/) | Monte Carlo tree search, policy and value networks, chess rules, and board representations. |
| [PythonRobotics](Copied/PythonRobotics/) | Quadrotor dynamics, trajectory generation, and trajectory control. |
| [Utama-Core](Copied/Utama-Core/) | Robot-soccer state, field geometry, object proximity, and state history. |
| [CRBot](Copied/CRBot-public/) | A game environment, visual observations, reward logic, and a DQN agent. |
| [CPHaskell](Copied/CPHaskell/) | Functional exercises, lazy streams, parser composition, and file-format parsers. |

Credit for these source implementations belongs to their original authors.

## Programming practice

The [practice collection](Leetcode/) contains exercises and reference material.

The [Kotlin practice ladder](Leetcode/LeetCode-in-Kotlin/PRACTICE-LADDER.md) gives an order for selected tasks. Some exercises use incomplete implementations that I fill in against tests.

The additional practice packs cover:

| Pack | Focus |
|---|---|
| [Collections and data structures](Leetcode/LeetCode-in-Kotlin/01-imperial40009/structures-pack-01/) | Linked lists, queues, ordered maps, frequency trees, and block-based text. |
| [Threads and tests](Leetcode/LeetCode-in-Kotlin/02-imperial40009/threaded-pack-02/) | Locks, concurrent operations, Java threads, and JUnit test cases. |
| [Java and Kotlin interoperability](Leetcode/LeetCode-in-Kotlin/03-imperial40009/junit-interop-pack-03/) | Cross-language interfaces, function references, and tests across Java and Kotlin. |

## Research papers

The [paper collection](Papers%20%5BReading%5D/) supports further study and related notes.

Selected papers include:

- [Mastering Chess and Shogi by Self-Play with a General Reinforcement Learning Algorithm](Papers%20%5BReading%5D/Printed/1712.01815v1.pdf).
- [Highly Accurate Protein Structure Prediction with AlphaFold](Papers%20%5BReading%5D/Printed/s41586-021-03819-2.pdf).
- [GR00T N1: An Open Foundation Model for Generalist Humanoid Robots](Papers%20%5BReading%5D/Robotics/2503.14734v2.pdf).
- [EgoScale: Scaling Dexterous Manipulation with Diverse Egocentric Human Data](Papers%20%5BReading%5D/Robotics/2602.16710v1.pdf).
- [Learning High-Speed Flight in the Wild](Papers%20%5BReading%5D/Robotics/Loquercio21_Science.pdf).
- [Proximal Policy Optimization Algorithms](Papers%20%5BReading%5D/To-Print/1707.06347v2.pdf).
- [Decision Transformer: Reinforcement Learning via Sequence Modeling](Papers%20%5BReading%5D/To-Print/2106.01345v2.pdf).

## Repository guide

| Location | Contents |
|---|---|
| [Copied](Copied/) | Dated study notes, exercises, source-code transcriptions, and supporting material. |
| [Agent artifacts](Copied/Agent_Artifacts/) | Interactive HTML lessons. |
| [Claudex](Copied/Claudex/) | Saved instructions for QSCHA, technical explanations, and session handoffs. |
| [Leetcode](Leetcode/) | Programming practice and reference solutions. |
| [Papers](Papers%20%5BReading%5D/) | Research-paper collection. |

The archive includes AI-generated explanations and exercises. I use these materials as part of the study process described above.

The interactive HTML lessons are also AI-generated study tools that I request and explore.

## License

See [LICENSE](LICENSE) for the repository license.

Third-party code and papers remain subject to their original license terms.