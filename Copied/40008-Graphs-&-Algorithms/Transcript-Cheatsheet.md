
---

### **Column 1: Graph Theory & Traversal**

**GRAPHS**
*   **Simple Graphs:** A graph is *simple* if it has no parallel arcs and no loops.
*   **Full (Induced) Subgraphs:** Any subset $X \subseteq \text{nodes}(G)$ induces a subgraph $G[X]$ of $G$, where $G[X]$ has nodes $X$ and $G[X]$ contains all arcs of $G$ which join nodes in $X$. $G'$ is a full (or induced) subgraph of $G$ if $G' = G[X]$ for some $X \subseteq \text{nodes}(G)$.
*   **Spanning Subgraphs:** If $G'$ is a subgraph of $G$ and $\text{nodes}(G') = \text{nodes}(G)$, we say that $G'$ *spans* $G$.
*   **Isomorphism:** Let $G, G'$ be graphs. An isomorphism from $G$ to $G'$ is a bijection $f: \text{nodes}(G) \rightarrow \text{nodes}(G')$ together with a bijection $g: \text{arcs}(G) \rightarrow \text{arcs}(G')$ such that if $a \in \text{arcs}(G)$ has endpoints $n_1$ and $n_2$ then the endpoints of $g(a)$ are $f(n_1)$ and $f(n_2)$.
*   **Sum of Degrees:** The sum of the degrees of all the nodes of a graph is twice the number of arcs, and therefore even.
*   **No. of Odd Nodes:** The number of nodes with odd degree is even.

**Isomorphism Example Problem:**
Graph $G_2$ is as shown in the diagram. How many isomorphisms are there from $G_2$ to itself (including the identity)? Justify your answer briefly.
*   > **Diagram Description for $G_2$:** A graph with 7 nodes numbered 1 through 7. Node 7 is in the dead center and is connected to nodes 1, 2, 3, 4, 5, and 6. Nodes 1 through 6 are connected to each other in a closed cycle (forming a hexagon shape around node 7).
*   *Answer:* Node 7 must be fixed since it is the only node with degree 6. Node 1 has 6 possible positions - fix 1. Then 7 is fixed. Node 3 has 4 - fix 3. Then 4 is fixed. Node 5 has 2 - fix 5. Then 6 is fixed. This gives 1 * 6 * 1 * 4 * 1 * 2 * 1 = 48 automorphisms.

**PLANARITY + COLOURING**
*   **Planar Graphs:** A graph is planar if it can be drawn so that no arcs cross.
*   **Homeomorphism:** Two graphs are homeomorphic if they can be obtained from the same graph by a series of operations where an arc $x-y$ is replaced by two arcs $x-z-y$.
*   **Bipartite Graphs:** $G$ is *bipartite* if $\text{nodes}(G)$ can be partitioned into sets $X$ and $Y$ in such a way that no two nodes of $X$ are joined and no two nodes of $Y$ are joined.
*   **Planar Graphs:** A graph is planar iff it does not contain a subgraph homeomorphic to $K_5$ or $K_{3,3}$.
*   **Euler's Formula (faces):** $F = A - N + 2$ for a connected planar graph.
*   **4 Colour Theorem:** Every map can be coloured using at most four colours.
*   **Colouring Proof Sketch:** Suppose that $G$ is 2-colourable. Show that $G$ has no odd length cycles. *Assume that $G$ has a 2-colouring col. Let $n_1, \dots, n_k, n_1$ be a cycle with $k$ odd. Then $\text{col}(n_1) = \text{col}(n_k)$, since colours must alternate along the cycle. This is a contradiction since $n_k$ is adjacent to $n_1$.*

**PATHS + CYCLES**
*   **Simple Paths:** A path is *simple* if it has no repeated nodes.
*   **Cycles:** A *cycle* (or circuit) is a path which (1) finishes where it starts, (2) has at least one arc, (3) does not use the same arc twice.
*   **Euler Path:** An Euler Path is a path which uses each arc exactly once.
*   **Euler Circuit:** An Euler Circuit/Cycle is a cycle which uses each arc exactly once.
*   **Hamiltonian Path:** visits every node exactly once.
*   **Hamiltonian Circuit:** a HP that returns to start node.
*   **Euler Path:** A connected graph has an Euler path iff there are 0 or 2 odd nodes.
*   **Euler Circuit:** A connected graph has an Euler circuit iff every node has even degree.

**TREES + DIRECTED GRAPHS**
*   **Tree:** A tree is an acyclic, connected, rooted graph.
*   **Spanning Tree:** Let $G$ be a graph. A non-rooted tree $T$ is said to be a spanning tree for $G$ if $T$ spans $G$, i.e. (1) $T$ is a subgraph of $G$ and (2) $\text{nodes}(T) = \text{nodes}(G)$.
*   **Directed Graph:** A directed graph is a set $N$ of nodes and a set $A$ of arcs such that each $a \in A$ is associated with an ordered pair of nodes (the endpoints of $a$).
*   **Strongly Connected:** A directed graph is *strongly connected* if for any $x, y \in \text{nodes}(G)$ there is a path from $x$ to $y$. (e.g. we need paths both from $x$ to $y$ and from $y$ to $x$).
*   **No. Of Nodes in Tree:** Let $T$ be a tree with $n$ nodes. Then $T$ has $n - 1$ arcs.

**GRAPH TRAVERSAL**

**DFS ($O(n+m)$):** Explores as far as possible along each branch before backtracking. Uses a stack (FILO).
```text
procedure dfs(x):
  visited[x] = true
  print x
  for y in adj[x]:
    if not visited[y]:
      parent[y] = x
      dfs(y)
```

**BFS ($O(n+m)$):** Explores all neighbours of a node before moving on to the next level. Uses a queue (FIFO).
```text
visited[x] = true; print x
enqueue(Q, x)
while not isEmpty(Q):
  y = front(Q)
  for z in adj[y]:
    if not visited[z]:
      visited[z] = true
      print z
      parent[z] = y
      enqueue(z, Q)
  dequeue(Q)
```

**Topological Sort (w/ DFS):** Given a directed acyclic graph (DAG) $G$ with $n$ nodes, find a total ordering of the nodes $x_1, \dots, x_n$ such that for any $i, j \le n$, if $i > j$ then there is no path from $x_i$ to $x_j$ in $G$. TS is the reverse order of exited nodes in DFS.
```text
procedure dfsts(x):
  entered[x] = true
  for y in adj[x]:
    if entered[y]:
      if not exited[y]:
        abort #cycle
    else:
      parent[y] = x
      dfsts(y)
  exited[x] = true
  ts[index] = x
  index = index - 1

index = n - 1
for x in nodes(G):
  if not entered[x]:
    dfsts(x)
```

---

### **Column 2: Minimum Spanning Trees & Shortest Path**

**MINIMUM SPANNING TREES + SHORTEST PATH PROBLEMS**
*   **Weighted Graphs:** A weighted graph $(G, W)$ is a simple graph $G$ together with a weight function $W: \text{arcs}(G) \rightarrow R^+ \text{ (reals } \ge 0)$.
*   **Union-Find:** Each set is stored as a (non-binary) tree. The root node is the representative (leader) of the set. We merge two sets by appending one tree to the other, so that the root of one tree is the child of the root of the other tree.
*   **Path Compression:** Improves UF by keeping tree depths low. When finding the root (leader) for a node $x$, if this is not $\text{parent}[x]$ then make $\text{parent}[y] = \text{root}$ for all $y$ on the path from $x$ to the root.

**Prim's ($O(n^2)$):**
Add the shortest arc which will extend the tree. (Greedy)
```text
Choose any node start as the root
tree[start] = true #initialise tree
for x in adj[start]: #initialise fringe
  fringe[x] = true #add x to fringe
  parent[x] = start
  weight[x] = W[start, x]
#end of initialisation

while not isEmpty(fringe):
  select fringe node f s.t. weight[f] is minimum
  fringe[f] = false
  tree[f] = true

  for y in adj[f]:
    if not tree[y]:
      if fringe[y]:
        if W[f, y] < weight[y]:
          weight[y] = W[f, y]
          parent[y] = f
      else:
        fringe[y] = true
        weight[y] = W[f, y]
        parent[y] = f
```

**Prim's (w/ Priority Qs) ($O(m \log n)$):**
Each item $x$ of the queue has a priority $\text{key}[x]$ which represents cost. Items removed lowest key first.
```text
Q = PQcreate()
for x in nodes(G):
  key[x] = ∞; parent[x] = null
  insert(Q, x)
decreaseKey(Q, start, 0)

while not isEmpty(Q):
  f = getMin(Q); deleteMin(Q)
  tree[f] = true
  for y in adj[f]:
    if not tree[y]:
      if W[f, y] < key[y]:
        decreaseKey(Q, y, W[f, y])
        parent[y] = f
```
*   > **Diagram Description for Prim's w/ Priority Qs:** Shows a 4-node square graph with a diagonal edge, tracing the state of node keys/parents over three steps as the tree builds. A small table tracks the node key values initialized to $\infty$, updating to weights (3, 5, 4, etc.) as the algorithm traverses.

**Kruskal's (w/ Union-Find) ($O(m \log n)$):**
At each stage choose the shortest arc not yet included, except when this would give a cycle. (Greedier)
```text
F = ∅
R = arcs(G)
while not isEmpty(R):
  remove a of smallest weight from R
  if a does not make a cycle when added to F:
    add a to F
return F
```
Can be implemented w/ PQ or by **dynamic equivalence classes**:
1. Put nodes in the same equivalence class if they belong to the same connected component of the forest constructed so far.
2. Map each node to the representative of its equivalence class.
3. An arc $\{x, y\}$ can only be added if $x$ and $y$ belong to different equivalence classes.
4. If $\{x, y\}$ is added, then merge the equivalence classes of $x$ and $y$.

Using **Union-Find** where each set has a leader element which is the representative of that set:
```text
Let G have n nodes numbered from 1 to n.
Build a PQ Q of the arcs of G with the weights as keys.
sets = UFcreate(n) #initialise Union-Find with singletons {1}, ..., {n}
F = ∅
while not isEmpty(Q):
  {x, y} = getMin(Q); deleteMin(Q)
  x' = find(sets, x) #finds the leader x' of x
  y' = find(sets, y)
  if x' != y': #no cycle
    add {x, y} to F
    union(sets, x', y')
```

**Dijkstra's ($O(n^2)$ or $O(m \log n)$):**
Finds the shortest paths from a source node to all other nodes. Very similar to Prim's - can be implemented using PQs.
```text
Q = PQcreate()
for x in nodes(G):
  key[x] = ∞; parent[x] = nil
  insert(Q, x)
decreaseKey(Q, start, 0)

while not tree[finish] and not isEmpty(Q):
  f = getMin(Q); deleteMin(Q)
  tree[f] = true
  for y in adj[f]:
    if not tree[y]: # so y in Q
      if key[f] + W[f, y] < key[y]:
        decreaseKey(Q, y, key[f] + W[f, y])
        parent[y] = f
```

**A*:**
Finds single pair shortest path problem - uses a heuristic $h(x)$ which underestimates the distance from any node $x$ to the finish node.
```text
Q = PQcreate()
for x in nodes(G):
  g[x] = ∞; key[x] = ∞; parent[x] = nil
  insert(Q, x)
g[start] = 0
decreaseKey(Q, start, g[start] + h[start])

while not tree[finish] and not isEmpty(Q):
  x = getMin(Q); deleteMin(Q)
  tree[x] = true
  for y in adj[x]:
    if not tree[y]: # so y in Q
      if g[x] + W[x, y] < g[y]:
        g[y] = g[x] + W[x, y]
        decreaseKey(Q, y, g[y] + h[y])
        parent[y] = x
```
*   **Consistent Heuristic:** A heuristic function is **consistent** if (1) for any adjacent nodes $x, y$ we have $h(x) \le W(x, y) + h(y)$ and (2) $h(\text{finish}) = 0$.
*   **Admissible Heuristic:** A heuristic function is **admissible** if for any node $x$ we have $h(x) \le$ the weight of the shortest path from $x$ to the goal finish.

**Floyd's + Warshall's ($O(n^3)$):**
Warshall's computes the transitive closure of a directed graph. Floyd's computes the shortest paths between all pairs of nodes in a weighted graph.
```text
Warshall's:
input A #adjacency matrix
copy A into B (array of Booleans) # B = B_0
for k = 1 to n: # B = B_{k-1}
  for i = 1 to n:
    for j = 1 to n:
      b_ij = b_ij or (b_ik and b_kj)
return B

Floyd's:
input A
set B[i, j] = { 0 if i = j
              { A[i, j] if i != j and there is an arc (i, j)
              { ∞ otherwise
# B = B_0
for k = 1 to n:
  for i = 1 to n:
    for j = 1 to n:
      b_ij = min(b_ij, b_ik + b_kj)
return B
```

---

### **Column 3: TSP & Complexity**

**TRAVELLING SALESMAN PROBLEM (TSP)**
*   **TSP:** Given a complete weighted graph $(G, W)$, find a way to tour the graph visiting each node exactly once and travelling the shortest possible distance.
*   **Bellman-Held-Karp ($O(n^2 2^n)$):** Solves the TSP. Uses dynamic programming like Floyd and Warshall.
```text
Input (G, W)
Choose start ∈ nodes(G)
for x ∈ nodes \ {start}:
  C[{x}, x] = W[start, x]

#Process sets S in increasing order of size.
for S ⊂ nodes \ {start} with S ≠ ∅:
  for x ∈ nodes \ (S ∪ {start}): #Find C[S, x]
    C[S, x] = ∞
    for y ∈ S:
      C[S, x] = min(C[S \ {y}, y] + W[y, x], C[S, x])

#Now have calculated and stored all values of C[S, x]

opt = ∞
for x ∈ nodes \ {start}:
  opt = min(C[Nodes \ {start, x}, x] + W[x, start], opt)
return opt
```

**COMPLEXITY (P and NP)**
*   **Tractability:** A problem is tractable iff it can be computed within polynomially many steps in worst case ($W(n) \le p(n)$ for some polynomial $p(n)$).
*   **Polynomial Invariance Thesis:** If a problem can be solved in p-time in some reasonable model of computation, then it can be solved in p-time in any other reasonable model of computation.
*   **P:** A decision problem $D(x)$ is in the complexity class P (polynomial time) if it can be decided within time $p(n)$ in some reasonable model of computation, where $n$ is the input size $|x|$.
*   **"Polynomially Bounded":** Suppose that $f$ is a p-time function. The output size $f(x)$ is polynomially bounded in the input size $|x|$: $|f(x)| \le p(|x|)$ for some polynomial $p(n)$. The reason is that any program which computes $f$ has only p-time in which to build the output.
*   **Function Composition:** Suppose that $f$ and $g$ are functions which are p-time computable. Then the composition $g \circ f$ is also p-time computable.
*   **NP:** A decision problem $D(x)$ is in NP (non-deterministic polynomial time) if there is a problem $E(x, y)$ in P and a polynomial $p(n)$ such that:
    (1) $D(x)$ iff $\exists y E(x, y)$
    (2) if $E(x, y)$ then $|y| \le p(|x|)$ (E is poly balanced)
*   **Reduction:** Suppose that $D$ and $D'$ are two decision problems. We say that $D$ (many-one) reduces to $D'$ ($D \le D'$) if there is a p-time computable function $f$ such that $D(x)$ iff $D'(f(x))$.
*   **NP-hard:** A decision problem $D$ is NP-hard if for all problems $D' \in NP$ we have $D' \le D$.
*   **NP-complete:** A decision problem $D$ is NP-complete (NPC) if:
    (1) $D \in NP$
    (2) $D$ is NP-hard

**(a) The problem HPA is defined as follows: given an undirected (simple) graph $G$ and an arc $a$ joining nodes $x$ and $y$ of $G$, is there a Hamiltonian path in $G$ which uses $a$? Explain why HPA belongs to the complexity class NP.**
We can nondeterministically guess a sequence of nodes $p$, then check in polynomial time that: (1) $p$ visits every node exactly once, (2) each consecutive pair in $p$ is connected by an edge in $G$, and (3) $p$ includes the given arc $a$. Formally, define $\text{VER-HPA}(G, x, y, p)$ iff "$p$ is a Hamiltonian path in $G$ that uses $a$". Since checking $\text{VER-HPA}$ can be done in polynomial time and $p$ is bounded in size by $G$, HPA belongs to NP because $\text{HPA}(G, x, y) \iff \exists p. \text{VER-HPA}(G, x, y, p)$.

**(b) Show that HPA is NP-complete. You may assume that the following problem HP is NP-complete: given a graph $G$, does $G$ have a Hamiltonian path?**
(b) We have shown that $\text{HPA} \in \text{NP}$ in part (a). To show that HPA is NP-hard, we show $\text{HP} \le \text{HPA}$.
Let $f(G)$ be $G$ with two new nodes $x, y$, with $x$ joined to every node of $G$, and with $y$ joined to $x$ by arc $a$. Then $f$ is clearly p-time.
*   > **Diagram Description for HPA Reduction:** The diagram shows a generic graph shape representing graph $G$ with arbitrary internal nodes $x_1, \dots, x_n$. Beside it, graph $f(G)$ shows a new node $x$ that draws edges to every single $x_n$ node inside $G$. An additional new node $y$ is attached exclusively to $x$ via the specific edge $a$. 

Suppose that $G$ has an HP $x_1, \dots, x_n$. Then $f(G)$ has an HP $y, x, x_1, \dots, x_n$ using $a$. Conversely, suppose that $f(G)$ has an HP using $a$. Clearly $y$ must be an endpoint since it has degree one. The HP must be of the form $y, x, x_1, \dots, x_n$. Then $x_1, \dots, x_n$ is an HP of $G$.












# SJFS


Here is a detailed transcription of the content in the image, organized by its major sections. Descriptions of the diagrams have been provided where applicable.

### SEARCHING A LIST
*   **Searching an unordered list L:** For $x$: if $x$ in $L$ return $k$ such that $L[k] = x$; if $x$ not in $L$ return "not found".
*   **Linear Search:** Inspect $L[0], L[1], \dots, L[n - 1]$ in turn. Stop and return index if $x$ found. Otherwise return "not found". $W(n) = n$
*   **Searching an ordered list L**
    *   **Modified Linear Search (left):** decision tree for $n=4$; $W(n) = n$
    *   **Binary Search (right):** decision tree for $n=8$; $W(n) = 1 + \lfloor \log n \rfloor$

> **Diagram Description (Binary Search Decision Tree):**
> A binary tree mapping the steps of a binary search for $n=8$. The root node compares the target $x$ with $L[3]$. 
> *   The left branch ($x < L[3]$) leads to a node checking $x$ against $L[1]$, which further splits into comparing against $L[0]$ or $L[2]$.
> *   The right branch ($x > L[3]$) leads to a node checking $x$ against $L[5]$, splitting into $L[4]$ and $L[6]$, with $L[6]$ further splitting to $L[7]$. 
> 
> The leaf nodes represent either finding the value at an index (e.g., $x=L[0]$) or a "fail" state. Below the tree, it notes $W(4) = 4$ (look at depth of tree) and $W(8) = 4$ (e.g., when $x = L[7]$).

---

### ORDERS
**Hierarchy of Orders:**
1. **Polynomials + Logs:** $1, \log n, n, n \log n, n^2, n^2 \log n, \dots$
2. **Exponentials:** $2^n, 3^n, 4^n \dots$
3. **Factorials:** $n!$

Let $f, g : \mathbb{N} \to \mathbb{R}^+$
1. $f$ is $O(g)$ iff $\exists m \in \mathbb{N} \ \exists c \in \mathbb{R}^+$ such that $\forall n \ge m \ f(n) \le c.g(n)$
2. $f$ is $\Theta(g)$ iff $f$ is $O(g)$ and $g$ is $O(f)$.

**Orders Example:** Let $f(n) = 6n^2 - n + 5$, and $g(n) = n^2/4 + 13n - 2$.
Show that $g \in O(f), f \in O(g)$.
Clearly $13n \le n^2$ for $n \ge 13$.
Hence 
$$g(n) = \frac{n^2}{4} + 13n - 2 \le \frac{n^2}{4} + n^2 = \frac{5n^2}{4}$$
for $n \ge 13$.
Also, $f(n) = 6n^2 - n + 5 \ge 5n^2$ for $n \ge 1$.
Thus, for $n \ge 13$, 
$$g(n) \le \frac{5}{4}n^2 \le 5n^2 \le f(n)$$
so $g \in O(f)$.

Now, $-n + 5 \le 5$ for all $n \ge 0$, so
$$f(n) \le 6n^2 + 5 \le 7n^2$$
for $n \ge 2$.
Also, $g(n) \ge \frac{n^2}{4}$ for $n \ge 1$.
Thus, for $n \ge 2$, 
$$7n^2 \le 28g(n) \implies f(n) \le 28g(n)$$
so $f \in O(g)$.

**Recurrence:** Solve the following recurrence relation:
$F(1) = 5; \quad F(n) = 3+F(\lfloor n/2 \rfloor)$
$$F(n) = 3 + F(\lfloor n/2 \rfloor)$$
$$= 3 + 3 + F(\lfloor n/4 \rfloor)$$
$$\dots$$
$$= 3 + 3 + \dots + 3 + F(1)$$
$$= 3 \lfloor \log(n) \rfloor + 5$$

---

### STRASSEN'S ALGORITHM
Assume we are multiplying two $n \times n$ matrices:
$$AB = C$$

Start with $n = 2$. Then
$$C = \begin{pmatrix} a_{11}b_{11} + a_{12}b_{21} & a_{11}b_{12} + a_{12}b_{22} \\ a_{21}b_{11} + a_{22}b_{21} & a_{21}b_{12} + a_{22}b_{22} \end{pmatrix}$$
This takes 8 multiplications (and 4 additions).

Strassen: can do $n = 2$ in only 7 multiplications (and 18 additions).
$$C = \begin{pmatrix} x_1 + x_4 - x_5 + x_7 & x_3 + x_5 \\ x_2 + x_4 & x_1 + x_3 - x_2 + x_6 \end{pmatrix}$$
where
$x_1 = (a_{11} + a_{22}) * (b_{11} + b_{22})$
$x_2 = (a_{21} + a_{22}) * b_{11}$
$x_3 = a_{11} * (b_{12} - b_{22})$
$x_4 = a_{22} * (b_{21} - b_{11})$
$x_5 = (a_{11} + a_{12}) * b_{22}$
$x_6 = (a_{21} - a_{11}) * (b_{11} + b_{12})$
$x_7 = (a_{12} - a_{22}) * (b_{21} + b_{22})$

Note that commutativity of multiplication is not used. Hence we can generalise to matrices.
Suppose that $n = 2^k$. Divide up matrices into four quadrants each $n/2 \times n/2$:
$$\begin{pmatrix} A_{11} & A_{12} \\ A_{21} & A_{22} \end{pmatrix} \begin{pmatrix} B_{11} & B_{12} \\ B_{21} & B_{22} \end{pmatrix} = \begin{pmatrix} C_{11} & C_{12} \\ C_{21} & C_{22} \end{pmatrix}$$
Compute $C_{ij}$ using the formulas for $c_{ij}$. Recursively compute each multiplication by further subdivision until bottom out at $n=2$.

*   **Strassen's algorithm:** splits matrices into $n/2 \times n/2$ blocks, does 7 recursive multiplications and 18 additions/subtractions. The recurrence is $A(n) = 7A(n/2) + 18(n/2)^2$, solving to $\Theta(n^{\log 7}) \approx \Theta(n^{2.807})$. If $n$ isn't a power of 2, pad with zeros to make it even.

---

### SORTING ALGORITHMS
*   **Insertion Sort** - $W(n)$ is $\Theta(n^2)$
    > **Diagram Description:** A horizontal bar representing an array divided into two sections: a "sorted" section from index $0$ to $i-1$, and an "unsorted" section from index $i$ to $n-1$. An arrow points from the unsorted section into the sorted section.

    Insert $L[i]$ into $L[0 \dots i-1]$ in correct position. Then $L[0 \dots i]$ is sorted. Insertion performed by letting $L[i]$ filter downwards by successive swaps. This takes between 1 and $i$ comparisons. Worst case when $L[i]$ below $L[0]$.

*   **MergeSort** - $W(n) = 2W(n/2) + (n-1)$
    > **Diagram Description:** A horizontal array split exactly in half at index $\lfloor \frac{n-1}{2} \rfloor$. Below it, a hand-drawn example shows an array `[2, 5, 6, 1, 8, 3, 0, 4]` being split and merged.

    1. Divide roughly into two.
    2. Sort each half separately (by recursion).
    3. Merge the two halves. We need to know how many comparisons must be done to merge the two halves. The merging will be done by comparing the current least elements of the lists, and outputting the smaller.

*   **QuickSort**
    Split the list around the first element. e.g.
    $L = [7, 2, 10, 12, 3, 1, 8]$
    Split around 7. Get
    $[3, 2, 1, 7, 12, 8, 10]$
    Now sort the two sides recursively. The list is then sorted:
    $[1, 2, 3, 7, 8, 10, 12]$
    *   Clearly split takes $n-1$ comparisons. QuickSort may well not split L evenly.
    *   $W(n) = \Theta(n^2)$ (same as insertion). $A(n) = \Theta(n \log n)$ (quite good in practice).

---

### LOWER BOUNDS
Idea: express sorting algorithm as decision tree.
The internal nodes are the comparisons.
The leaves are the results (the rearranged lists).
We then argue that to have a certain number of leaves, the tree must have sufficient depth.
Depth = worst-case number of comparisons

**For example: 3-Sort Algorithm**
For the root node, if Item 1 is smaller than item 2, go down left path, otherwise go down right path.
Then compare item 2 and 3.

> **Diagram Description (3-Sort Decision Tree):**
> A color-coded tree diagram mapping the paths to sort 3 items. Internal nodes (red) represent boolean comparisons like `(1,2)` or `(2,3)`. The leaf nodes (green) show all $3! = 6$ possible sorted permutations of an array: `[1,2,3]`, `[1,3,2]`, `[3,1,2]`, `[2,1,3]`, `[2,3,1]`. 

*   If a binary tree has depth $d$ then it has $\le 2^d$ leaves.
*   Any decision tree for sorting a list of length 3 must have $3! = 6$ leaves.
*   Cannot have depth $\le 2$, as all binary trees of depth $\le 2$ have $\le 4$ leaves.
*   So depth at least 3. Worst-case number of comparisons at least 3.
*   Hence 3-Sort (and Insertion Sort) are optimal for $n = 3$.

Decision tree has $\ge n!$ leaves and depth $d$.
So 
$2^d \ge n!$
$d \ge \log(n!)$
$d \ge \lceil \log(n!) \rceil$

**Lower bound for sorting in worst case:**
Any algorithm for sorting by comparisons must perform at least $\lceil \log(n!) \rceil$ comparisons in worst case.

---

### HEAPSORT (and more on trees)
*   **Total Path Length:** The total path length of a tree is the sum of the depths of all leaf nodes.
*   **Balanced Trees:** A tree of depth $d$ is balanced if every leaf is at depth $d$ or $d - 1$.
*   **Heap Structure:** is a left-complete binary tree.
*   **Left-complete:** means that if the tree has depth $d$ then (1) all nodes are present at depth $0, 1, \dots, d - 1$ and (2) at depth $d$ no node is missing to the left of a node which is present.

> **Diagram Descriptions (Tree Structures):**
> *   **Tree 1:** Shows a left-complete binary tree mapping to an array. The root is 3. Its children are 9 and 13. This continues down level by level, filling from left to right. Below the tree is the corresponding array: `[3, 9, 13, 0, 12, 4, 8, 16, 7, 1, 5, 6, 2]` mapped to indices 1 through 13.
> *   **Tree 2 (Min Heap):** A tree illustrating the "minimising partial order tree" property. The root is 0, and every parent node is smaller than its children (e.g., 0 has children 16 and 9).
> *   **Tree 3 (Max Heap):** A tree illustrating the "maximising partial order tree" property. The root is 16, and every parent node is larger than its children (e.g., 16 has children 12 and 13).

*   A tree T is a **minimising partial order tree** if the key at any node $\le$ the keys at each child node (if any).
*   A **min heap** is a heap structure with the min partial order tree property.
*   A tree T is a **maximising partial order tree** if the key at any node $\ge$ the keys at each child node (if any).
*   A **max heap** is a heap structure with the max partial order tree property.

**Pseudocode Blocks:**
*   **Heapsort scheme**
    Build max heap H out of an array E of elements
    for $i = n$ to $1$:
    max = getMax(H)
    deleteMax(H)
    E[i] = max
*   **getMax(H)** - just read the root node of H
*   **deleteMax(H) scheme**
    copy element at last node into root node
    remove last node
    fixMaxHeap(H)
*   **buildMaxHeap(H) scheme**
    if H not a leaf:
    buildMaxHeap(left subtree of H)
    buildMaxHeap(right subtree of H)
    fixMaxHeap(H)
*   **fixMaxHeap(H) scheme**
    if H not a leaf:
    largerSubHeap = the left or right subheap with the larger root
    if root(H).key < root(largerSubHeap).key:
    swap elements at root(H) and root(largerSubHeap)
    fixMaxHeap(largerSubHeap)

---

### MASTER THEOREM
Let $T(n) = aT(n/b) + f(n)$
has solutions as follows,
where $E = \log a / \log b$ is the critical exponent:
1. If $n^{E+\epsilon} = O(f(n))$ for some $\epsilon > 0$ then $T(n) = \Theta(f(n))$.
2. If $f(n) = \Theta(n^E)$ then $T(n) = \Theta(f(n) \log n)$.
3. If $f(n) = O(n^{E-\epsilon})$ for some $\epsilon > 0$ then $T(n) = \Theta(n^E)$.

For each of the following recurrence relations, use the Master Theorem to obtain a solution up to $\Theta$. In each case state the critical exponent E and explain your answer briefly.
**(a) $T_1(n) = 4T_1(n/2) + 8n$**
(a) $a = 4$, $b = 2$, $f(n) = 8n$, $E = \log a / \log b = \log 4 / \log 2 = 2$.
Clearly $f(n) = O(n^{2-\epsilon})$ (taking $\epsilon = 1/2$) and so $T_1(n) = \Theta(n^2)$.