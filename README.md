# Project Liberty

My central library for ongoing technical study and programming practice.

> "Sometimes taking time is actually a shortcut."

This repository records how I move from an unfamiliar concept to an independent solution.

I personally love typing out explanations (be it from the internet, or Gemini) 
and source code manually so I can pace myself. Because I find that if I do 
read-only, I would start moving too quickly and get burnt out quickly. 

I use questions and repeated practice to develop my understanding. I also ask AI tools to make interactive lessons that connect equations to visual examples.

I add material as my studies and projects develop. The library has no fixed completion date.

[Structure](#repository-structure) · [Interactive lessons](#interactive-lessons) · [QSCHA](#qscha) · [Subjects](#subjects-in-the-library) · [Codebase studies](#codebase-studies)

## Repository structure

```text
Project-Liberty/
├── README.md
├── LICENSE
├── study-notes/
│   ├── 40001-Computer-Systems/
│   ├── 40005-Computer-Architecture/
│   ├── 40007-Databases/
│   ├── 40008-Graphs-&-Algorithms/
│   ├── 40016-Calculus/
│   ├── 40017-Linear-Algebra/
│   ├── 40018A-Discrete_Maths/
│   ├── 40018B-Logic/
│   ├── 40018C-Reasoning/
│   ├── Imperial-Miscellaneous/
│   ├── NVIDIA-Webinars/
│   ├── Project-Altiera/
│   └── Project-Automaton/
├── practice/
│   ├── haskell/
│   ├── kotlin/
│   ├── numpy/
│   └── sql/
├── interactive-lessons/
│   ├── Essence-of-Similarity-Transforms/
│   ├── Essence-of-SO3-SE3-Geodesic-Error/
│   ├── Essence-of-BOP-Pose-Errors/
│   └── Similarity-Transforms-Intuitive.html
├── code-studies/
│   ├── llama.cpp/
│   ├── AlphaZero_Chess/
│   ├── PythonRobotics/
│   ├── Utama-Core/
│   ├── CRBot-public/
│   ├── CPHaskell/
│   └── ART - MLSG/
├── papers/
│   ├── Printed/
│   ├── Robotics/
│   ├── To-Print/
│   ├── Unprinted/
│   └── ...
├── ai-workflows/
│   ├── standard-technical-claude-skills/
│   ├── standard-technical-codex-skills/
│   └── ...
├── personal-notes/
│   └── Douglas.md
└── reference-repos/                 # Local only; excluded from Git
    ├── In-Progress/
    ├── Completed/
    └── Planned/
```

| Location | Purpose |
|---|---|
| [Study notes](study-notes/) | Technical explanations, worked examples, and project-related notes. |
| [Practice](practice/) | Language exercises, problem sets, and solution attempts. |
| [Interactive lessons](interactive-lessons/) | Visual HTML lessons with controls and mathematical explanations. |
| [Code studies](code-studies/) | Manual transcriptions of selected source code from existing projects. |
| [Papers](papers/) | Research papers for current and future study. |
| [AI workflows](ai-workflows/) | Instructions for QSCHA, technical explanations, and session handoffs. |
| [Personal notes](personal-notes/) | Personal notes outside the main technical subjects. |

The local `reference-repos/` folder contains reference repositories and related material. It is excluded from Git.

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

For mathematics, syntactical hints give the notation and relevant definitions. 
Conceptual hints give the proof strategy or calculation steps.

For programming, at times I don't even know what the logical flow to solving a 
task even is, let alone it's syntaxes. So conceptual hints would help show me
what the logical essence that's needed, before I start working with the forest
of syntax.

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

The [QSCHA method document](ai-workflows/standard-technical-codex-skills/skills/standard-technical-qscha/references/qscha-engine.md) contains the detailed format.

## Interactive lessons

Additionally, I like asking Fable or Astra to make visual explanations for 
concepts that I want to understand. For example, to learn about pose estimation
of Computer Vision, I find that it's much more effective to ask these agents to 
create me interactive video-game-like artifacts, for which I can tweak with such
that I'm able to more intuitively understand the essence of a topic. I find this
builds up intuition much better over being obfuscated and confused by syntax and overly
technical terms.

Examples:

| Lesson | Topics |
|---|---|
| [Essence of Similarity Transforms](interactive-lessons/Essence-of-Similarity-Transforms/index.html) | Matrix columns, coordinate transforms, uniform scale, and the image transforms used in GigaPose. |
| [Essence of Rotations and Rigid Motions](interactive-lessons/Essence-of-SO3-SE3-Geodesic-Error/index.html) | SO(3), axis-angle rotation, exponential and logarithmic maps, SE(3), and pose error. |
| [Essence of BOP Pose Errors](interactive-lessons/Essence-of-BOP-Pose-Errors/index.html) | MSSD, MSPD, VSD, object symmetry, visibility, and pose evaluation. |

The chapters include:

- Controls for the visual examples.
- Questions to answer before each experiment.
- Mathematical explanations.
- Comparisons between related concepts.

For example, the similarity-transform lessons examine two different uses of the word “similar.” One concerns a change of basis. The other concerns a geometric transformation with uniform scale.

The BOP lessons compare how different metrics measure the same pose error.

The links above open HTML source files on GitHub. To use the lessons, download the repository and open the relevant `index.html` in a web browser.

## Subjects in the library

### Computer science and mathematics

| Subject | Examples in the notes |
|---|---|
| [Computer systems](study-notes/40001-Computer-Systems/) | Boolean algebra, Karnaugh maps, latches, flip-flops, finite-state machines, and processor datapaths. |
| [Computer architecture](study-notes/40005-Computer-Architecture/) | RISC-V instruction formats, ALU design, processor control, pipeline hazards, x86-64 assembly, and caches. |
| [Graphs and algorithms](study-notes/40008-Graphs-%26-Algorithms/) | Graph isomorphism, BFS, DFS, shortest paths, minimum spanning trees, dynamic programming, and NP-completeness. |
| [Databases](study-notes/40007-Databases/) | Relational algebra, functional dependencies, normalization, transaction schedules, concurrency control, and Datalog. |
| [SQL practice](practice/sql/) | Joins, keys, NULL semantics, aggregates, window functions, and entity–relationship models. |
| [Haskell](practice/haskell/) | Recursion, algebraic data types, folds, lazy evaluation, typeclasses, parser combinators, and monads. |
| [Calculus](study-notes/40016-Calculus/) | Limits, continuity, Riemann sums, convergence, Taylor series, and metric spaces. |
| [Linear algebra](study-notes/40017-Linear-Algebra/) | Gaussian elimination, vector spaces, rank and nullity, eigenvalues, changes of basis, and orthogonal projection. |
| [Discrete mathematics](study-notes/40018A-Discrete_Maths/) | Relations, equivalence classes, partial orders, functions, cardinality, and countability. |
| [Logic](study-notes/40018B-Logic/) | Propositional logic, first-order logic, natural deduction, quantifiers, and normal forms. |
| [Program reasoning](study-notes/40018C-Reasoning/) | Mathematical induction, structural induction, proof obligations, loop invariants, and termination. |

### Robotics, machine learning, and software tools

The [Project Automaton notes](study-notes/Project-Automaton/) connect robotics concepts to practical software and simulation work.

| Subject | Examples in the notes |
|---|---|
| [Robot simulation and control](study-notes/Project-Automaton/) | MuJoCo C API, position control, finite-state controllers, simulation stability, and insertion-score calculations. |
| [Robot geometry and perception](study-notes/Project-Automaton/) | Coordinate frames, Jacobians, singularities, quaternions, SLERP, and ChArUco camera calibration. |
| [Machine learning and numerical tools](study-notes/Project-Automaton/) | NumPy arrays, transformer attention, KV caches, low-rank methods, flow matching, and robot foundation models. |
| [Robotics platforms](study-notes/NVIDIA-Webinars/) | Isaac Sim, Isaac Lab, synthetic data, robot deployment, and Jetson hardware. |
| [AI-agent architecture](study-notes/Project-Altiera/) | OpenClaw architecture, tool interfaces, plugins, memory search, WebSockets, and Android build workflows. |
| [Additional study material](study-notes/Imperial-Miscellaneous/) | Robot software interview practice and presentation diagrams about AI distillation and its wider effects. |

The dated files preserve questions, partial attempts, and later explanations. They show the steps of the study process.

## Selected notes

These files give concrete examples of that process:

- [From vectors to an insertion score](study-notes/Project-Automaton/June-14.ipynb)  
  Notes connect vector projection and lateral error to a robot insertion benchmark.

- [Camera calibration and ChArUco boards](study-notes/Project-Automaton/July-16/July-16.ipynb)  
  Notes examine corner identifiers and the structure of calibration code.

- [Parser combinators in Haskell](practice/haskell/Dec-30_v3.hs)  
  Practice combines small parsers through `Functor`, `Applicative`, and `Alternative`.

- [Loop invariants and proof obligations](study-notes/40018C-Reasoning/Apr-18.ipynb)  
  Notes connect program statements to the conditions necessary for a correctness proof.

- [Graph structure through visual examples](study-notes/40008-Graphs-%26-Algorithms/23-Jan.ipynb)  
  Notebook examples use NetworkX and Matplotlib to examine graph structure and symmetry.

## Codebase studies

Manual source-code transcription remains part of the method.

I use it to examine how an existing implementation represents data and divides responsibilities between functions. These folders contain study copies of code from other projects.

| Codebase | Material in this library |
|---|---|
| [llama.cpp and GGML](code-studies/llama.cpp/) | Public interfaces, tensor operations, architecture definitions, batches, and LoRA adapters. |
| [AlphaZero Chess](code-studies/AlphaZero_Chess/) | Monte Carlo tree search, policy and value networks, chess rules, and board representations. |
| [PythonRobotics](code-studies/PythonRobotics/) | Quadrotor dynamics, trajectory generation, and trajectory control. |
| [Utama-Core](code-studies/Utama-Core/) | Robot-soccer state, field geometry, object proximity, and state history. |
| [CRBot](code-studies/CRBot-public/) | A game environment, visual observations, reward logic, and a DQN agent. |
| [CPHaskell](code-studies/CPHaskell/) | Functional exercises, lazy streams, parser composition, and file-format parsers. |

Credit for these source implementations belongs to their original authors.

## Programming practice

The [practice collection](practice/) contains exercises and reference material.

| Folder | Material |
|---|---|
| [Haskell](practice/haskell/) | Functional programming exercises and detailed explanations. |
| [Kotlin](practice/kotlin/) | Algorithm problems, data structures, Java interoperability, and tests. |
| [NumPy](practice/numpy/) | Starter material for numerical programming practice. |
| [SQL](practice/sql/) | Query exercises and database notes. |

The [Kotlin practice ladder](practice/kotlin/PRACTICE-LADDER.md) gives an order for selected tasks. Some exercises use incomplete implementations that I fill in against tests.

The additional practice packs cover:

| Pack | Focus |
|---|---|
| [Collections and data structures](practice/kotlin/01-imperial40009/structures-pack-01/) | Linked lists, queues, ordered maps, frequency trees, and block-based text. |
| [Threads and tests](practice/kotlin/02-imperial40009/threaded-pack-02/) | Locks, concurrent operations, Java threads, and JUnit test cases. |
| [Java and Kotlin interoperability](practice/kotlin/03-imperial40009/junit-interop-pack-03/) | Cross-language interfaces, function references, and tests across Java and Kotlin. |


## License

See [LICENSE](LICENSE) for the repository license.

Third-party code and papers remain subject to their original license terms.
