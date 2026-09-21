# Page 1



Here is a complete, extremely detailed transcription of the cheat sheet provided in the image. Since the entire image is a dense text-based study guide containing formulas, code, and definitions rather than standard visual diagrams or numeric data tables, I have formatted everything hierarchically using Markdown. Mathematical symbols have been transcribed using standard notation, and SQL/Datalog examples are placed in code blocks.

---

## Column 1: Relational Algebra (RA)

## Relations

* $R(A, B, \dots)$: $R =$ name, $A, B \dots = $ attributes ($\vec{A}$).
* **Arity ($n$):** Number of attributes.
* **Extent:** Set of tuples $\{ \langle v_1^A, v_1^B \rangle, \dots \}$.
* **Semantics:** No duplicates. Row/col order does not matter. All tuples share same arity.

## Keys

* **Key:** Subset of attributes with unique values across all tuples in $R$. Violated if 2 tuples match.
* **Superkey:** If $K$ is a key, $KR$ is also a key.
* **Minimal Key:** No subset is also a key.
* **Primary Key:** The explicitly chosen default key.
* **Foreign Key:** $R(\vec{X}) \xrightarrow{FK} S(\vec{Y})$. Values in $\vec{X} \subseteq R$ must appear in $\vec{Y} \in S$, where $\vec{Y}$ is a key of $S$.

## Relational Algebra (RA) Basics

* **Unary:** $\pi$ (Project/Cols), $\sigma$ (Select/Rows), $\rho$ (Rename).
* **Binary:** $\times$ (Product), $\cup$ (Union), $-$ (Difference).
* **Rules:** Output of nested operator must contain attributes required by outer $\pi$ or $\sigma$. Inputs to $\cup$ or $-$ must have the exact same number of attributes.

## Operations & Intuitive Meanings

* **Cardinality** ($|R|$): Total number of tuples.
* **Natural Join** ($R \bowtie S$): Matches rows with equal values in identically named columns. Drops duplicate columns. *Intuition: "Combine related data."*
* **Equi Join:** $R \bowtie_{A=B} S = \sigma_{R.A=S.B}(R \times S)$
* **Semi Join** ($R \ltimes S$): $R \bowtie \pi_{Attr(R) \cap Attr(S)}(S)$. *Intuition: Acts as a pure filter. Returns rows from R that have at least one match in S, but DOES NOT include any columns from S.*
* **Theta Join:** $R \bowtie_{\theta} S = \sigma_{\theta}(R \times S)$. *Intuition: Cross product filtered by ANY condition (e.g., $R.A < S.B$).*
* **Intersection:** $R \cap S = R - (R - S)$
* **Division:** $R \div S$. *Intuition: "Find the X that has ALL the Ys."*

> **PATTERN: Division Queries ($\div$)**
> Use when question asks for *"suppliers who supply ALL parts"* or *"students who take ALL courses."*
> If $R$ is `Suppliers(sid, pid)` and $S$ is `Parts(pid)`: $R \div S$ returns `(sid)` of suppliers who supply every part in $S$.

> **PATTERN: Finding the Maximum/Minimum**
> *Find the employee with the highest salary.* (Strategy: Find everyone who is LESS than someone else, then subtract them from everyone).
> 1. Find losers: $L = \pi_{E1.id}(E1 \bowtie_{E1.sal < E2.sal} \rho_{E2}(E1))$
> 2. Subtract: $\pi_{id}(E) - L$
> 
> 

> **PATTERN: Simulating Left Outer Join in RA**
> *Sometimes exams ask you to simulate an outer join using basic RA operators.*
> $R \text{ LEFT OUTER JOIN } S = (R \bowtie S) \cup \Big( \big( R - \pi_{Attr(R)}(R \bowtie S) \big) \times \{ (null, null, \dots) \} \Big)$

> **TRAP: Cartesian Product Explosion**
> Forgetting the join condition ($\theta$) after a cross product ($\times$) results in every row matching every row. Always check that relations in a $\times$ are properly filtered, or use $\bowtie$ instead.

> **TRAP: Self-Joins without Renaming**
> If you join a table to itself, you MUST use $\rho$ (rename) to differentiate the columns (*e.g., $\rho_{S1}(Student) \times \rho_{S2}(Student)$*). Otherwise, RA cannot resolve $Student.id = Student.id$.

## Equivalences

**Projects ($\pi$):**

* $\pi_{\vec{X}}\pi_{\vec{Y}}R \equiv \pi_{\vec{X}}R$ *(Eliminate inner $\pi$)*
* $\pi_{\vec{X}}\sigma_{P(\vec{Y})}R \equiv \sigma_{P(\vec{Y})}\pi_{\vec{X}}R$ (if $\vec{Y} \subseteq \vec{X}$)
* $\pi_{\vec{X}}(R \times S) \equiv \pi_{\vec{X} \cap A(R)}R \times \pi_{\vec{X} \cap A(S)}S$
* $\pi_{\vec{X}}(R \cup S) \equiv \pi_{\vec{X}}R \cup \pi_{\vec{X}}S$
* $\pi_{\vec{X}}(R - S) \supseteq \pi_{\vec{X}}R - \pi_{\vec{X}}S$

**Selects ($\sigma$):**

* $\sigma_{P(\vec{X})}\pi_{\vec{X}}R \equiv \pi_{\vec{X}}\sigma_{P(\vec{X})}R$
* $\sigma_{P_X(\vec{X})} \sigma_{P_Y(\vec{Y})} R \equiv \sigma_{P_X(\vec{X}) \wedge P_Y(\vec{Y})} R$
* $\sigma_{P(\vec{X})}(R \times S) \equiv \sigma_{P(\vec{X})}R \times S$ (if $\vec{X} \subseteq A(R)$)
* $\sigma_{P(\vec{X})}(R \cup S) \equiv \sigma_{P(\vec{X})}R \cup \sigma_{P(\vec{X})}S$
* $\sigma_{P(\vec{X})}(R - S) \equiv \sigma_{P(\vec{X})}R - \sigma_{P(\vec{X})}S$

**Sets ($\times, \cup, -$):**

* $R \times (S \cup T) \equiv (R \times S) \cup (R \times T)$
* $R \times (S - T) \equiv (R \times S) - (R \times T)$
* $R - (S \cup T) \equiv (R - S) - T$
* *Note: Cannot move $\cup$ or $-$ inside $\times$. Cannot move $\times$ inside $-$.*

> **EXAMPLE: Example RA Query**
> *Find names of customers at the Strand branch with positive rates:*
> $\pi_{cname}(\sigma_{rate > 0 \wedge bname='Strand'}(account \bowtie branch))$

---

## Column 2: Core & Advanced SQL

## SQL Data Types & DDL

* **Types:** BOOLEAN, INTEGER, DECIMAL(p,s), CHAR(n), VARCHAR(n), DATE, TIMESTAMP, ARRAY, MULTISET, XML.
* **Create:** ```sql
CREATE TABLE R (a INT NOT NULL, b VARCHAR(5),
CONSTRAINT pk PRIMARY KEY (a),
CONSTRAINT fk FOREIGN KEY (b) REFERENCES S);
```

```


* **Alter PK:** `ALTER TABLE R ADD CONSTRAINT... PRIMARY KEY (A);`
* **Sec. Key:** `CREATE UNIQUE INDEX idx ON R(A);`

## DML (Insert, Update, Delete)

* `INSERT INTO R VALUES (1, 'a'), (2, 'b');`
* `UPDATE R SET A = 1 WHERE B = 2;`
* `DELETE FROM R WHERE A = 1;`

## SQL to RA Equivalences

* `SELECT DISTINCT A FROM R WHERE P` $\equiv \pi_A \sigma_P (R)$
* **Bag Semantics:** `SELECT` (**default** = ALL) does not eliminate duplicates. `DISTINCT` forces Set semantics.
* `CROSS JOIN` (or $R, S$) $\equiv \times$
* `NATURAL JOIN` $\equiv \bowtie$ (Joins on same-named attrs)
* `JOIN S ON \theta` $\equiv \bowtie_\theta$
* `JOIN S USING (A)` joins purely on shared attr $A$.
* `UNION`, `EXCEPT`, `INTERSECT` $\equiv \cup, -, \cap$. (Defaults to DISTINCT. Requires union-compatibility).

> **PATTERN: The Anti-Join (Finding Missing Data)**
> *Find accounts with NO movements:*
> ```sql
> SELECT a.no FROM account a LEFT JOIN movement m ON a.no = m.no WHERE m.mid IS NULL;
> 
> ```
> 
> 

> **PATTERN: Self-Joins**
> *Find employees earning more than their managers:*
> ```sql
> SELECT e.name FROM emp e JOIN emp m ON e.mgr_id = m.id WHERE e.sal > m.sal;
> 
> ```
> 
> 

> **EXAMPLE: Finding Duplicates using GROUP BY**
> *Find names that appear more than once in the table:*
> ```sql
> SELECT name, COUNT(*) FROM Users GROUP BY name HAVING COUNT(*) > 1;
> 
> ```
> 
> 

> **EXAMPLE: Basic Inner Join**
> *Find distinct names of accounts with matching branches:*
> ```sql
> SELECT DISTINCT a.cname FROM account a JOIN branch b USING (sortcode) WHERE a.rate IS NOT NULL;
> 
> ```
> 
> 

## Advanced Queries & Subqueries

* **Aliases:** `SELECT a AS b FROM R AS r_1`
* **CTEs:** `WITH T AS (SELECT...) SELECT ... FROM T`
* **IN / EXISTS:** `IN` tests set membership. `EXISTS` tests if a subquery returns $\ge 1$ row.
* **Correlated:** Subquery references outer query cols. It executes row-by-row for the outer query.
* `V op SOME S`: TRUE if $\ge 1$ match in set.
* `V op ALL S`: TRUE if all items match in set.

> **EXAMPLE: Correlated Subquery**
> *Find employees who earn MORE than the average salary of THEIR OWN department:*
> ```sql
> SELECT e1.name, e1.salary FROM Emp e1 WHERE e1.salary > (
> SELECT AVG(e2.salary) FROM Emp e2 WHERE e2.dept = e1.dept);
> 
> ```
> 
> 

> **TRAP: NULLs in NOT IN vs NOT EXISTS**
> *If a subquery returns ANY NULL values, NOT IN evaluates to UNKNOWN for everything and filters out ALL rows. NOT EXISTS ignores NULLs safely. Always prefer NOT EXISTS when nullable columns are involved.*

---

## Column 3: Logic, Functions, Grouping & Window

> **PATTERN: Division in SQL (Double NOT EXISTS)**
> *Find students who have taken ALL courses:*
> ```sql
> SELECT s.name FROM Student s WHERE NOT EXISTS (
> SELECT c.cid FROM Course c WHERE NOT EXISTS (
> SELECT * FROM Enrol e WHERE e.sid = s.sid AND e.cid = c.cid) );
> 
> ```
> 
> 

## NULLs & 3-Valued Logic

* Truth values: **TRUE, FALSE, UNKNOWN**.
* `x = NULL` evaluates to UNKNOWN. Instead, use `x IS NULL` or `x IS NOT NULL`.
* `NOT UNKNOWN` = UNKNOWN.
* `UNKNOWN AND FALSE` = FALSE.
* `UNKNOWN OR TRUE` = TRUE.
* **Equivalence Note:** `EXCEPT` $\equiv$ `NOT EXISTS` $\equiv$ `NOT IN` **ONLY IF** attributes are not nullable.

## SQL Functions & CASE

* **Matching:** `LIKE` pattern `ESCAPE 'e'`. `_` = 1 char, `%` = 0+ chars.
* **General:** `COALESCE(v1, v2...)` = 1st non-NULL. `GREATEST/LEAST(v1, v2...)` = max/min (returns NULL if any is NULL). `CAST(v AS type)`.
* **String:** `CHAR_LENGTH(s)`, `TRIM(LEADING|TRAILING p FROM s)`, `POSITION(sub IN s)`, `SUBSTRING(s FROM start FOR n)`, `s1 || s2` (concat), `UPPER(s)`, `LOWER(s)`.
* **Math:** `ROUND(n, dp)`, `POWER(n, exp)`, `SQRT`, `ABS`, `LN`, `LOG10`, Trigs (`SIN`, `COS`, etc.).
* **CASE:** `CASE WHEN cond THEN v1 ELSE v2 END`.

> **EXAMPLE: Conditional Aggregation (COUNT FILTER)**
> *Count how many 'Pass' and 'Fail' grades exist per course in one query:*
> ```sql
> SELECT cid,
> COUNT(grade) FILTER (WHERE grade >= 50) AS passes,
> COUNT(grade) FILTER (WHERE grade < 50) AS fails
> FROM Enrolment GROUP BY cid;
> 
> ```
> 
> 

## Outer Joins

* **LEFT JOIN:** Keeps all R. Unmatched S cols are NULL.
* **RIGHT JOIN:** Keeps all S. Unmatched R cols are NULL.
* **FULL OUTER JOIN:** Keeps all R and S.

## Aggregation & Grouping

* **Aggregates:** `SUM, COUNT, AVG, MIN, MAX, ANY_VALUE, ARRAY_AGG`. *Note: NULL attributes are ignored!* `COUNT(*)` counts rows, `COUNT(attr)` counts non-null values.
* **GROUP BY:** 1 row per group. All non-aggregated cols in SELECT must be in GROUP BY.
* **HAVING:** Filters groups. Executed *after* GROUP BY, *before* SELECT.

> **TRAP: WHERE vs HAVING**
> *WHERE filters rows BEFORE grouping. HAVING filters groups AFTER aggregation. You cannot put SUM(x) > 10 in a WHERE clause!*

## Transactions & Ordering

* **OLTP** (Transactions, few rows) vs **OLAP** (Analytics, many rows).
* `BEGIN TRANSACTION ... COMMIT.`
* `ORDER BY col [ASC|DESC].`
* **Limit results:** `FETCH FIRST n ROWS ONLY.`

## Window Functions (OVER)

Creates a window to apply aggregates without collapsing rows.

* **Syntax:** `agg_func() OVER (PARTITION BY c1 ORDER BY c2 ROWS BETWEEN...)`
* **Ranking:** `RANK()` (skips ties), `DENSE_RANK()` (no skips), `ROW_NUMBER()`.
* **Offsets:** `LEAD(A)` (next), `LAG(A)` (prev), `FIRST_VALUE, LAST_VALUE`.
* **Sliding Window:** `ROWS BETWEEN [UNBOUNDED] PRECEDING AND [UNBOUNDED] FOLLOWING | CURRENT ROW`.

> **EXAMPLE: Running Totals (Cumulative Sum)**
> *Calculate the running total of sales for each employee ordered by date:*
> ```sql
> SELECT emp_id, s_date, SUM(amount) OVER (PARTITION BY emp_id ORDER BY s_date) AS run_total FROM Sales;
> 
> ```
> 
> 

## Advanced OLAP & CTEs

* **ROLLUP(cols):** Generates subtotals and grand totals.
* **CUBE(cols):** Generates all subtotal combinations.
* **Pivot** (Rows $\rightarrow$ Cols): Use FILTER.
`COUNT(no) FILTER (WHERE type='current') AS curr`
* **Un-pivot** (Cols $\rightarrow$ Rows): Use `UNION` to stack columns into a triple format (ID, ColName, Value).
* **Recursive CTE:** Runs until no new rows are added.
```sql
WITH RECURSIVE T AS (BaseCase UNION RecursiveStep) 
SELECT * FROM T;

```



> **EXAMPLE: Window & CTE**
> *Rank accounts by rate within each branch:*
> ```sql
> WITH valid_accs AS (SELECT * FROM account WHERE rate > 0)
> SELECT sortcode, cname, RANK() OVER (PARTITION BY sortcode ORDER BY rate DESC) FROM valid_accs;
> 
> ```
> 
> 

> **EXAMPLE: Recursive CTE (Graph Reachability)**
> ```sql
> WITH RECURSIVE Ancestor AS (
> SELECT parent, child FROM Family WHERE child = 'John' -- Base
> UNION
> SELECT f.parent, a.child FROM Family f JOIN Ancestor a ON f.child = a.parent -- Step
> ) SELECT * FROM Ancestor;
> 
> ```
> 
> 

---

## Column 4: Datalog

## Fundamentals

* **Extensional Predicates:** Facts (the actual data). *e.g., branch(B6, 'Wimbledon').*
* **Intensional Predicates:** Rules. `Head :- Body.` (If Body is true, Head is true). Head must be a single predicate. Body is a conjunction (AND) of predicates.
* **Syntax Rules:**
* Variables start with a **Capital** letter.
* Predicates/constants start with a **lowercase** letter.
* `_` = anonymous variable (used when a variable only appears once).
* Cannot share names between intensional and extensional predicates.


* **Minimal Model:** The minimum set of predicates that must be true given the rules and facts.
* **Safe Negation:** Any variable appearing in a negated predicate ($\neg$) must also appear in a positive (non-negated) predicate within the same rule body.

> **TRAP: Safe Negation & Infinite Domains**
> *Unsafe: `ans(X) :- \neg r(X).` (What is X? It could be anything in the universe!)*
> *Safe: `ans(X) :- domain(X), \neg r(X).*`

> **TRAP: Stratified Negation**
> *If a rule uses negation, the negated predicate must be FULLY computable before this rule fires. You cannot have circular dependency chains involving negation.*

## RA to Datalog Equivalences

* **Project ($\pi$):** Achieved by using a subset of the body variables in the rule Head.
`ans(X) :- r(X, _).`
* **Select ($\sigma$):** Achieved by putting a data constant in the body, comparing variables, or repeating a variable name.
`ans(X, Y) :- r(X, Y), X > 1000.`
* **Product ($\times$):** Achieved by naming two independent predicates in the rule body.
`ans(X, Y, A, B) :- r(X, Y), s(A, B).`
* **Join ($\bowtie$):** Achieved by sharing the same variable name across multiple predicates in the body.
`ans(X, Y, Z) :- r(X, Y), s(Y, Z).`
* **Union ($\cup$):** Achieved by writing more than one rule definition for the exact same intensional Head predicate.
`ans(X) :- r(X).`
`ans(X) :- s(X).`
* **Difference ($-$):** Achieved using negation ($\neg$) on the subtracted predicate.
`ans(X) :- r(X), \neg s(X).` *(Note: If B in A - B is complex, B must be written as its own separate rule first).*
* **Division ($\div$):** (e.g., Find customers with ALL account types). Requires a double-negation pattern (Find those missing a type, then subtract them).
`missing_type(C) :- acc(_, Type), acc(C, _), \neg acc(C, Type)`
`has_all(C) :- acc(C, _), \neg missing_type(C).`

> **PATTERN: Finding the Maximum (Datalog)**
> *Strategy: Find all elements that are strictly less than another, then find elements NOT in that set.*
> `less(X) :- r(X), r(Y), X < Y.`
> `max(X) :- r(X), \neg less(X).`

> **EXAMPLE: Transitive Closure (Paths)**
> *Find all nodes reachable from node X in a graph:*
> `path(X,Y) :- edge(X,Y). -- Base step`
> `path(X,Y) :- edge(X,Z), path(Z,Y). -- Recursive step`



---

# Page 2



Here is a highly detailed transcription of the database cheat sheet, organized by the conceptual sections presented in the image.

---

### **EXAMPLE: Datalog Rule**

Find names of customers with a 'deposit' account > 5.0 rate:
`high_rate_cust(Name) :- account(_, 'deposit', Name, Rate, _), Rate > 5.0.`

---

### **ER Modelling**

#### **Core Elements**

* **Entity (Rectangle):** A set of conceptual objects of the same type.
* **Relationship (Diamond):** A set of tuples forming an association between entities.
* **Cardinality:** Upper bound U (max times an instance appears). Lower bound L (min times). e.g., $1:1$, $0:N$.

> **[Diagram Description: Simple ER Diagram]**
> A visual representation showing two rectangular entities, "person" and "dept", linked by a diamond-shaped relationship labeled "works". The cardinality line from "person" has a '1', and the line from "dept" has an 'N', indicating a one-to-many relationship.

> **TRAP: Fan & Chasm Traps**
> * **Fan Trap:** Two $1:N$ relationships fan out from the same entity, making it impossible to unambiguously link the entities on the 'N' sides together.
> * **Chasm Trap:** A pathway exists conceptually between entities, but optional ($0:N$) relationships break the chain, leaving some entities orphaned in queries.
> 
> 

#### **Attributes & Keys**

* **Mandatory:** Each instance must have exactly one value.
* **Optional (?):** The value can be null/missing.
* **Multi-valued (*, +):** Multiple values are permitted.
* **Key (Underlined):** Uniquely identifies the entity.

> **[Diagram Description: Entity Attributes]**
> A central rectangle labeled "staff" surrounded by circular/oval attributes attached by lines. The attribute "ni" is underlined (primary key). "bonus?" is marked as optional. "phone+" is marked as multi-valued. "sal" is a standard mandatory attribute.

#### **Extended ER Constructs**

* **Subset/ISA Hierarchy ($ER^S$):** Arrow from subset to superset ($E_s \subseteq E$).
* **Generalisation ($ER^D$):** Denotes disjointness. Subclasses are mutually exclusive.
* **Weak Entity ($ER^W$):** Double-lined. Its key relies on its participation in a relationship. Must have at least one key attribute and one 1:1 identifying relationship.
* **n-ary / Nested ($ER^{N:N}$):** Edges connecting $> 2$ entities, or linking directly to other relationships.

> **[Diagram Description: Complex ER Diagram]**
> This diagram illustrates extended constructs. It shows a "manager" entity with an ISA arrow pointing up to a "person" entity, indicating a subset/superset relationship. Below "person", a double-lined diamond ("has") connects to a double-lined rectangle ("dependant"), illustrating a weak entity depending on a strong entity.

#### **Relational Mapping (Table Per Type)**

* **Entities:** Map to a table. Key attributes become the Primary Key (PK).
* **Many-to-Many ($0:N$ to $0:N$):** Create a new table containing the PKs of both entities as Foreign Keys (FKs).
* **One-to-Many ($1:1$ to $0:N$):** Add a FK column in the 'N' side table pointing to the '1' side table's PK.
* **Weak Entities:** The PK of the parent entity becomes a FK in the weak entity's table, and forms part of the weak entity's composite PK.
* **Multi-valued Attr:** Stored in a new table containing the entity's PK + the attribute value. Both form the new PK.

> **EXAMPLE: Weak Entity Mapping**
> If *Building*(`bname`) is $1:N$ with *Room*(`rno`), and *Room* is weak.
> **Mapping:** `Building(bname)`, `Room(bname, rno)`. The primary key of the weak entity is always a composite of its own key + the parent's key.

> **PATTERN: ISA Hierarchy Relational Mapping Alternatives**
> 1. **Table per Class:** Parent table has PK. Child tables have same PK (acting as FK to parent). Cleanest, preserves all rules, but requires JOINS.
> 2. **Single Table:** One massive table with all child attributes and a "Type" discriminator column. Lots of NULLs.
> 3. **Table per Concrete Class:** Push parent attributes down into separate child tables. Bad if parent is queried globally.
> 
> 

---

### **Functional Dependencies & Normalisation**

#### **Functional Dependencies (FDs)**

* **Definition:** $X \to Y$. If two tuples agree on attributes $X$, they must agree on attributes $Y$.
* **Superkey:** An attribute set $X$ that functionally determines all other attributes in $R$.
* **Minimal Key:** A superkey where removing any attribute means it is no longer a superkey.
* **Prime Attribute:** An attribute that is part of *any* minimal candidate key. All others are *non-prime*.

#### **Armstrong's Axioms & Rules**

* **Reflexivity:** If $Y \subseteq X$, then $X \to Y$ (Trivial FD).
* **Augmentation:** If $X \to Y$, then $XZ \to YZ$.
* **Transitivity:** If $X \to Y$ and $Y \to Z$, then $X \to Z$.
* **Union:** If $X \to Y$ and $X \to Z$, then $X \to YZ$.
* **Decomposition:** If $X \to Y$ and $Z \subseteq Y$, then $X \to Z$.
* **Pseudotransitivity:** If $X \to Y$ and $WY \to Z$, then $WX \to Z$.

> **PATTERN: Finding Minimal Keys Algorithm**
> 1. Find attributes that never appear on the RHS of any FD. These MUST be in every candidate key.
> 2. Find closure of these mandatory attributes. If it equals all attributes, you're done.
> 3. If not, add combinations of other attributes one by one and test closures until you find minimal sets that yield all attributes.
> 
> 

#### **Closures & Canonical Cover**

* **Attribute Closure ($X^+$):** The set of all attributes functionally determined by $X$ under a set of FDs $S$.
* **FD Closure ($S^+$):** The set of all FDs that can be inferred from $S$.
* **Minimal/Canonical Cover ($S_C$):** A simplified set of FDs where $S_C^+ = S^+$. No FD can be removed, and no attribute from an FD can be removed, without changing the closure.

> **PATTERN: Finding Canonical Cover ($S_C$) Algorithm**
> 1. **Right-Split:** Ensure every FD has only one attribute on the RHS ($A \to BC \implies A \to B, A \to C$).
> 2. **Left-Reduce:** For FDs with multiple attributes on LHS ($AB \to C$), check if removing $A$ or $B$ changes the closure. If $A^+ \supset C$, then $B$ is redundant.
> 3. **Redundant FDs:** Temporarily remove an FD ($X \to Y$). Calculate $X^+$ using the remaining FDs. If $Y$ is still in the closure, the FD is redundant and can be permanently deleted.
> 
> 

> **EXAMPLE: Finding Canonical Cover ($S_C$) Detailed Step-by-Step**
> Given FDs: $A \to BC$, $B \to C$, $A \to B$, $AB \to C$. Find $S_C$.
> **Step 1. Right-Split:** Make RHS singletons.
> $A \to B, A \to C, B \to C, A \to B, AB \to C \implies A \to B, A \to C, B \to C, AB \to C$.
> **Step 2. Remove Duplicates:**
> $A \to B, A \to C, B \to C, AB \to C$.
> **Step 3. Left-Reduce (LHS simplification):**
> Look at $AB \to C$. Does $A \to C$ alone? Find $A^+$ (ignoring $AB \to C$). $A^+ = \{A, B, C\}$. Yes, $C$ is in the closure! So $B$ is redundant. $AB \to C$ becomes $A \to C$.
> **Step 4. Redundant FDs (RHS checks):**
> We have $A \to B, A \to C, B \to C$. Can we drop $A \to C$?
> Find $A^+$ using ONLY the remaining rules ($A \to B, B \to C$).
> $A^+ = \{A, B, C\}$. Since $C$ is still determined by $A$ via transitivity, $A \to C$ is completely redundant.
> **Final Canonical Cover ($S_C$):** $A \to B, B \to C$.

#### **Normal Forms**

* **1st Normal Form (1NF):** Every attribute depends on the key (no redundant repeating groups/structures).
* **3rd Normal Form (3NF):** For every non-trivial FD $X \to A$, **either** $X$ is a superkey, or $A$ is a prime attribute.
* *Rule of thumb:* "Every non-key attribute depends on the key, the whole key, and nothing but the key."
* *Generation:* Find FD $X \to A$ violating 3NF. Decompose $R$ into $R_a(Attr(R) - A)$ and $R_b(XA)$. Repeat.
* *FD Preservation:* It is always possible to decompose to 3NF in a way that is lossless and preserves all FDs.



> **PATTERN: 3NF Synthesis Algorithm (Ensures FD Preservation)**
> 1. Find the Canonical Cover ($S_C$).
> 2. For each FD $X \to Y$ in $S_C$, create a table $R_i(XY)$.
> 3. If none of the created tables contains a candidate key of the original relation, create one more table containing just the candidate key.
> 4. Eliminate any table that is a strict subset of another.
> 
> 

* **Boyce-Codd Normal Form (BCNF):** Stricter than 3NF. For every non-trivial FD $X \to A$, $X$ must be a superkey.

> **TRAP: BCNF vs FD Preservation**
> Decomposing to BCNF might result in a schema that does NOT preserve all original FDs. e.g., $R(A, B, C)$, Key: $AB$. FDs: $AB \to C$, $C \to B$. $C \to B$ violates BCNF. Decompose to $R_1(C, B)$ and $R_2(A, C)$. The FD $AB \to C$ is lost!

#### **Lossless Join Decomposition**

A decomposition of $R$ into $R_1, \dots, R_n$ is lossless if joining them back together yields the exact original relation $R$ (no phantom tuples).

* **Rule:** If $R(A_1 \dots A_n)$ has FD $A_j \to A_{j+1} \dots A_n$, decomposing into $R_1(A_1 \dots A_j)$ and $R_2(A_j A_{j+1} \dots A_n)$ is strictly lossless.

> **EXAMPLE: Checking Lossless Join (2 Tables)**
> To check if splitting $R$ into $R_1$ and $R_2$ is lossless, find their intersection: $X = R_1 \cap R_2$.
> **Rule:** The decomposition is Lossless IF AND ONLY IF $X \to R_1$ OR $X \to R_2$ exists in the FD closure $F^+$. (i.e. the shared attributes form a superkey for at least one of the tables).

---

### **Concurrency Control**

#### **Transactions & ACID**

A transaction is an indivisible logical unit of work.

* **Atomicity:** All or nothing. If a transaction fails, its partial effects are completely undone.
* **Consistency:** Moves the database from one valid state to another.
* **Isolation:** Transactions execute independently; intermediate states are invisible to others.
* **Durability:** Once committed, changes are permanent and survive crashes.

#### **Histories & Schedules**

* **History ($H$):** Sequence of read ($r_i[x]$), write ($w_i[x]$), commit ($c_i$), or abort ($a_i$) operations for transaction $T_i$ on object $x$.
* **Serial Execution:** One transaction completely finishes before the next begins. (Always maintains ACID).
* **Concurrent Execution:** Interleaving of operations from different transactions.

#### **Anomalies**

Problems that arise from bad interleavings.

* **Lost Update (LU):** $T_1$ and $T_2$ read the same object, then both update it. One update overwrites the other. ($r_1[x] \dots r_2[x] \dots w_1[x] \dots w_2[x]$).
* **Inconsistent Analysis (IA):** $T_1$ reads multiple values while $T_2$ modifies them, leading $T_1$ to see an inconsistent aggregate state.
* **Dirty Read (DR):** $T_2$ reads a value written by $T_1$, but $T_1$ later aborts. $T_2$ relied on invalid data.
* **Dirty Write (DW):** $T_2$ overwrites a value written by an uncommitted $T_1$.

In the table below, $c_i$ means either $c_i$ or $a_i$ occurring, and $op_a < op_b$ means $op_a$ happens before $op_b$ in the history.

| Anomaly | Set | Pattern | Problem |
| --- | --- | --- | --- |
| **Dirty Write** | DW | $w_1[x] < w_2[x] < c_1$ | Sometimes not SR |
| **Dirty Read** | DR | $w_1[x] < r_2[x] < c_1$ | Sometimes not RC |
| **Inconsistent Analysis** | IA | $r_1[x] < w_2[x], w_2[y] < r_1[y]$ | Not SR |
| **Lost Update** | LU | $r_1[x] < w_2[x] < w_1[x]$ | Not SR |

#### **Serialisability (SR)**

Ensures Isolation. A concurrent history is serialisable if its final outcome is identical to *some* serial execution of those same transactions.

* **Conflict:** Two operations belong to different transactions, access the same object, and at least one is a write. (e.g., $r_1[x] \to w_2[x]$, or $w_1[x] \to r_2[x]$, or $w_1[x] \to w_2[x]$).
* **Conflict Serialisable (CSR):** A history is CSR if it is conflict-equivalent to a serial history.
* **Serialisation Graph (SG):** Nodes are transactions. Directed edge $T_1 \to T_2$ if there is a conflict where $T_1$'s operation precedes $T_2$'s.
* **Rule:** If the $SG(H)$ has **no cycles**, the history is Conflict Serialisable (CSR).

> **PATTERN: Reading a Schedule to build an SG**
> 1. Read operations left to right.
> 2. For every $r_i[x]$ or $w_i[x]$, scan forward to find any $w_j[x]$ or $r_j[x]$ (where $i \neq j$).
> 3. If found, draw an arrow $T_i \to T_j$. Do this for ALL conflicting pairs.
> 4. If no cycles exist, any valid topological sort gives you the equivalent serial schedule order!
> 
> 

#### **Recoverability (RC)**

Deals with aborted transactions.

* **Recoverable (RC):** If $T_2$ reads from $T_1$, $T_1$ MUST commit before $T_2$ commits. (Prevents committing on bad data).
* **Avoids Cascading Aborts (ACA):** Transactions only read from *already committed* transactions. (Prevents chain-reaction rollbacks).
* **Strict (ST):** Transactions neither read nor write an object until the transaction that last wrote it has committed/aborted.
* *Hierarchy:* **ST $\subset$ ACA $\subset$ RC**.

> **EXAMPLE: Identifying Recoverability**
> **Scenario:** $w_1[X], r_2[X], c_1, c_2$
> **Analysis:** $T_2$ read uncommitted data, but $T_1$ committed before $T_2$ committed. This is **Recoverable (RC)** but NOT ACA.
> **Scenario:** $w_1[X], c_1, r_2[X], c_2$
> **Analysis:** $T_2$ read data AFTER $T_1$ committed, and did not overwrite any uncommitted data from $T_1$. This is **STRICT (ST)**.

#### **Two-Phase Locking (2PL)**

A protocol to guarantee Conflict Serialisability.

* **Read Lock ($rl[x]$):** Shared lock.
* **Write Lock ($wl[x]$):** Exclusive lock.
* **Rule 1 (Growing):** A transaction can acquire locks but cannot release any.
* **Rule 2 (Shrinking):** Once a transaction releases its first lock, it cannot acquire any new locks.
* **Why it works:** It forces a "maximum lock period" for all items, preventing conflict cycles.

**Rules for locking:**

* Refuse $rl_i[x]$ if $wl_j[x]$ already held
* Refuse $wl_i[x]$ if $rl_j[x]$ or $wl_j[x]$ already held

#### **Deadlocks in 2PL**

* 2PL can cause deadlocks ($T_1$ waits for $T_2$ to release a lock, while $T_2$ waits for $T_1$).
* **Waits-For Graph (WFG):** Edge $T_1 \to T_2$ if $T_1$ is waiting for a lock held by $T_2$.
* **Rule:** A cycle in the WFG means a deadlock has occurred. One transaction must be aborted to break the cycle.

> **TRAP: Deadlocks in 2PL**
> 2PL guarantees Serialisability, but it DOES NOT prevent deadlocks. Deadlocks must be handled externally (e.g., timeouts, cycle detection).