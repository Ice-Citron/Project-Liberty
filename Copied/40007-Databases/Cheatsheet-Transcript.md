# Page 1

Here is a highly detailed transcription of the cheat sheet provided in the image. I have structured it column by column, transcribed the table into Markdown format, and provided detailed paragraphs describing the diagrams as requested.

---

### **Left Column**

**ACID Transactions**
**START TRANSACTION** to start a transaction, **COMMIT** to end transaction

* **Atomicity/Durability:** Recovery system allows rollback of aborted transactions. Maintains a log of all changes including old values. RAID (Redundant Array of Independent/Inexpensive Disks) uses constant replica disks of the DB, one of which can takes over upon disk failures.
* **Consistency:** implemented by the user or through DBMS/SQL constraints
* **Isolation:** Concurrency Control System allows for concurrency via 4 isolation levels for the user to choose from (goal is maximise concurrency while maintaining serializability):
* **read uncommitted:** uncommitted data changed by other transactions can be read (dirty reads) - all changes by other transactions are immediately visible (lowest isolation)
* **read committed:** only data committed by another transaction can be read
* **repeatable read:** like read committed, but guarantees all rows returned by a query will be included if we repeat the query (however, may return new tuples (phantom reads))
* **serializable:** trivial - guarantees isolation but limits concurrency (slow and wastes resources) this is the point of a **concurrency control system**.



No level allows dirty writes (updating modified but uncommitted data).

| level | dirty reads | non-repeatable reads | phantom reads |
| --- | --- | --- | --- |
| read uncommitted | possible | possible | possible |
| read committed | prevented | possible | possible |
| repeatable read | prevented | prevented | possible |
| serializable | prevented | prevented | prevented |

*(Note: There is a small photograph of a man's face located to the right of this table).*

**Schedules:**

* **Recoverable:** no transaction T in S commits until all transactions T' that have written an item that T reads have committed.
* **Cascadeless:** where every transaction reads only the items that are written by committed transactions (no uncommitted updated data can be read)
* **Serial:** for every transaction T participating in the schedule, all the operations of T are executed consecutively in the schedule.
* **Serializable:** it is equivalent to some serial schedule of the same n transactions.
* **Conflict:** 2 instructions of different transactions conflict iff there exists some item Q that both instructions access and at **least one writes to**. Intuitively, conflicting instructions forces an order between them, and non-conflicting ones can be interchanged without affecting the state. 2 schedules are:
* **Result Equivalent** if they produce the same final state of the DB.
* **Conflict Equivalent** if the order of any 2 conflicting operations are the same in both schedules.
* **Conflict Serializable** if it is conflict equivalent to some serial schedule S'.



**Precedence Graph:** Nodes are transactions T1..Tn; we draw an arc from Ti to Tj if there exists a conflict between them and Ti's conflicting instruction is scheduled to execute before Tj's. A schedule is **conflict-serializable iff the graph is acyclic**.

**Indexing**
Stored in their own relation as search-key/pointer pairs.

* **Ordered index** - index entries stored in order by search-key value
* **Primary index** - (aka clustering index) in a sequentially ordered file, the index whose search key specifies the sequential order of the file. Usually primary key.
* **Secondary index** - an index whose search key specifies an order different from the sequential order of the file.
* **Dense index** - index record appears for every search-key value in the file.
* **Sparse index** - contains index records for only some search-key values. Applicable only when records are sequentially ordered on search key.
* **Multilevel index** - in case primary index does not fit in memory, we create another sparse index (outer index) for the primary index (inner index). If outer index is too large to fit either, another level can be created for the outer index, and so on.
* **Sparse index deletion** - If deleted key value exists in the index, replace this value by the next search-key value in the file. If this next search-key value already has an index entry, delete it instead of replacing it.
* **Sparse index insertion** - no change needs to be made to the index unless a new block is created, in which case the first search-key value appearing in the new block is inserted into the index.
* **Secondary indices** point to their records via buckets; must be dense.

---

### **Middle Column**

***Diagram Description: Dense Index***

> This diagram illustrates a Dense Index structure. On the left is an index file where every single search key (e.g., A-217, A-101, A-110, A-215, etc.) has an explicit entry. Next to each search key is a pointer (an arrow) that maps directly to the corresponding record in the main data file on the right. The data file contains complete records like "Brighton 750", "Downtown 500", etc. The key takeaway is a 1:1 ratio between index entries and data records.

***Diagram Description: Sparse Index***

> This diagram shows a Sparse Index. The main data file on the right is divided into sequential blocks. The index file on the left does not contain every key. Instead, it only contains an entry for the first search key of each block in the data file (e.g., "Brighton", "Mianus", "Redwood"). Pointers from these select index entries direct the system to the start of the corresponding data block, requiring sequential scanning within the block to find specific, un-indexed records.

***Diagram Description: Multilevel Index***

> This diagram visualizes a two-tier indexing system. On the far left is an "outer index" block. This outer index acts as a sparse index for an "inner index" layer. The pointers from the outer index lead to several different blocks of the inner index. Finally, the pointers from the inner index blocks lead to the actual data blocks. This demonstrates how to manage an index that has grown too large to fit entirely into main memory by indexing the index itself.

***Diagram Description: Secondary Index with buckets***

> This diagram displays how a Secondary Index functions. On the left is the secondary index, ordered by a specific search key (e.g., numbers like 350, 400, 500). Because the main data file on the right is ordered differently (likely by a primary key), the secondary index pointers do not go straight to the data. Instead, they point to an intermediate layer of "buckets" in the middle. These buckets contain lists of pointers that then fan out to the various dispersed locations in the data file where records matching the secondary key reside.

**B-trees**
Can be used as a more efficient store of indices:

* Perfectly balanced - each **leaf** node is at **same depth**
* Every internal node (except root) must be at least half full, where **t-1 < no. keys in node < 2t**
* Any B-tree with n nodes, height h and min. degree t satisfies **h ≤ log_t((n+1)/2)**

**Insertion:** Insert like any tree insertion. If overflow occurs, split the node in half and insert the middle value into the parent node. This can propagate up e.g. if this causes the parent node to overflow. Complexity: **O(h) = O(log_t(n))**

***Diagram Description: B-tree Insertion***

> A sequence of small, graphical tree nodes demonstrates the insertion process and the resulting overflow split. It starts with a leaf node containing values `[ 39 | 50 | 60 ]`. Text above says `Insert 61:`, and an arrow points to a temporary state labeled `OVERFLOW` showing `[ 39 | 50 | 60 | 61 ]` packed into the node. Because the node is full, it undergoes a `SPLIT IT` operation. The middle value, '50', is pushed up to the parent node. The final state shows the parent node now containing `[ ... | 50 | ... ]`, which branches down to two new sibling leaf nodes: `[ 39 ]` on the left and `[ 60 | 61 ]` on the right.

**Deletion:** Delete like any tree deletion. If underflow occurs, merge with its sibling and pull the parent node's in-between key into the merged node (c). Can also do this without merging on 1 single node (a/b)

***Diagram Description: B-tree Deletion***

> A complex set of three multi-step tree diagrams (labeled a, b, and c) illustrates different underflow scenarios during deletion.
> * **Case (a)** shows borrowing from a sibling: A node loses a key and underflows. It borrows a key from its sibling by pulling a value down from the parent and pushing a sibling's value up to replace it in the parent.
> * **Case (b)** shows replacing a deleted key with a successor: The diagram shows finding the next sequential key in the child branches to replace a key deleted from an internal node.
> * **Case (c)** shows a full merge due to underflow: When a node underflows and its sibling cannot spare a key, the two siblings are merged. A value from the parent node (e.g., '50') is pulled down to join the remaining keys of the underflowed node and its sibling (e.g., '45' and '70') into a single new, larger node.
> 
> 

**B+ trees:** A variation where you string all leaf nodes together, and replicate keys from non-leaf nodes into leaf nodes to make sure every key appears at leaf level.

**Hashing:** Ideal hash function is **uniform** (each bucket assigned same no. search-key values) and **random** (each bucket has same no. records assigned to it regardless of actual distribution of search-key values in file)

---

### **Right Column**

**Storage:**
A typical transaction server consists of multiple processes accessing data in **shared memory**.

***Diagram Description: Transaction Server Architecture***

> This flowchart illustrates the architecture of a database transaction server. At the top, three "user process" boxes send requests down to three corresponding "server process" boxes. These server processes interact with a large central block labeled "shared memory". Inside the shared memory are four components: "buffer pool", "query plan cache", "log buffer", and "lock table". Surrounding and interacting with the shared memory are specific system processes: a "lock manager" controls the lock table, a "process monitor" oversees operations, a "db writer process" moves data from the buffer pool to external disk storage, and a "log writer process" and "checkpoint process" handle writing logs and checkpoints from the log buffer to stable storage.

* The **server** receives user queries, which are processed and executed in the db writer process before sending results back.
* **Lock Manager** locks records for concurrency.
* **DB writer** outputs the modified buffer to disks.
* **Log writer** receives logs from server processes, which it adds to the buffer and writes to stable storage.
* **Checkpoint process** performs periodic checkpoints.
* Process Monitor monitors other processes, and takes recovery action on failures.

To ensure that no 2 processes are accessing the same data structure at the same time in shared memory, DBMS's implement **mutual exclusion** using OS semaphores or atomic instructions.

In order of **storage hierarchy** (primary/secondary/tertiary):

* **Cache** - fastest and most costly, managed by computer system hardware.
* **Main Memory** - fast access, generally too small to store whole DB, volatile.
* **Flash** - Non-volatile (survives power failure), fast reads, slow and limited writes. **NOR** flash has fast reads, very slow erase, has lower capacity, generally used to store program code in embedded devices. **NAND** flash uses page-at-a-time read/write, multi-page erase, high capacity, and widely used as data storage in portable devices.
* **Magnetic disk** - primary medium for long term data storage, data must be moved through main memory, direct access (read in any order), survives power failure and system crashes.
* **Optical** - data read using a laser on spinning disk, slower read/writes than magnetic disk.
* **Tape** - usually used for backups and archives - much slower than disk, very high capacity

***Diagram Description: Magnetic Disk Assembly***

> A 3D schematic diagram of a hard disk drive's internal physical components. It shows multiple flat, circular "platters" stacked vertically on a central rotating "spindle". Data is arranged on the platters in concentric circles labeled as a "track", and a small wedge of a track is labeled a "sector". The same track number across all stacked platters is designated vertically as a "cylinder". An "arm assembly" extends to the side, featuring multiple arms that reach between the platters. At the tip of each arm, hovering just above the platter surface, is a "read-write head" used to access the data.

**Disk controller:** interfaces between system/disk.
Accepts read/write commands, **moves arm** to right track, reads/writes data. Verifies data with **checksums**. Reads back sector post-write to ensure correctness. **Remaps** bad sectors.

* *Access time = seek time (move arm) + rotational latency (wait for sector)*
* *Data transfer rate = speed of reading/writing data*

**Block:** contiguous sectors on one track.

We can **optimise block access** by **minimising arm movement, e.g., elevator algorithm:** arm one direction until no more requests in that direction, then reverse.
**File organisation on disk:** store related data **close together**; avoid **fragmentation**.
**Non-Volatile Write buffers** write to RAM; reorders writes to minimise arm movement. **Log disk** performs sequential logging to avoid seeks. **Journaling file systems** write updates in safe order, otherwise risk of corruption to file system data.

**Storage Access:** Keep **frequently used blocks** in memory.
**Buffer:** main memory for disk block copies. **Buffer manager:** allocates/replaces buffer space; writes back only if modified.
**Buffer Replacement policies** can be **LRU** (least recently used) or:
**Pinned:** don't write back; **Toss-immediate:** free immediately; **MRU:** pin latest used; **Statistical:** usage-based

**File Organisation of DBs:**
*(Note: There is a small photograph of a man's face next to this heading).*
**Fixed-length** records: uniform size, may need shifting/deletion links.
**Variable-length** records: slotted pages with headers pointing to records.
**Sequential File Organisation:** Ordered by search key; use **pointer chains to delete**; and when inserting, reuse space if available, else use overflow block.

**Record Storage Methods:**
**Heap:** arbitrary placement; **Sequential:** ordered by key; **Hashing:** Uses a hash function to specify block to place in; **Multitable Clustering file organisation:** we store related records from multiple tables together.
**Row-oriented** data layout is efficient for **full-record** queries.
**Column-oriented** data layout is efficient for **single-attribute** queries.
**Sorting** optimises for faster queries (e.g., where attr > val).












---

# Page 2

Here is a highly detailed transcription of the database concepts cheat sheet provided in the image, organized by its main columns and sections.

---

### Column 1: Entity-Relationship (ER) Modeling

**Cardinality & Relationships**

* **1 to 1:** A person can only drive 1 car, a car can only be driven by 1 person.
* *Diagram Description:* An entity rectangle labeled "person" connects via a line to a diamond relationship labeled "drive", which connects to an entity rectangle labeled "car". Both connecting lines have the number "1" above them.
* *Text:* Can choose table to put foreign key in: `person(ID, reg)` `car(reg)`


* **1 to Many:** A person can drive many cars, a car can only be driven by 1 person.
* *Diagram Description:* Similar to the previous diagram, but the line connecting "drive" to "car" has an "N" above it, and the person line has a "1".
* *Text:* Foreign key in many table: `person(ID)` `car(reg, pID)`
* *(Note: There is a small photo of a man's face next to this section).*


* **Many to Many:** A person can drive many cars, a car can be driven by many people.
* *Diagram Description:* The connecting lines for the "drive" relationship have an "M" on the person side and an "N" on the car side.
* *Text:* Composite table created: `person(ID)` `car(reg)` `drive(pID, reg)`. Any relationship attributes go in the many table (car).



**Attribute Types**

* **Composite attributes:**
* *Diagram Description:* An attribute oval labeled "address" branches out into three sub-attribute ovals: "road", "city", and "postcode".
* *Text:* Sub-attributes are flattened: `Actor(road, city, postcode)`


* **Derived attributes (dashed):**
* *Diagram Description:* An entity labeled "actor" connects to a dashed-line oval labeled "age".
* *Text:* Cannot be represented in a relational schema. E.g. age from DOB.


* **Multivalued attributes (double circle):**
* *Diagram Description:* An entity labeled "actor" connects to a double-line oval labeled "phones".
* *Text:* mapped back to their own relation via a foreign key: `actor(ID)` `actor_cars(actorID, carID)`



**Participation**

* **Total Participation (double line):** All entities in an entity set must participate in the relation. (Here, every person drives at least 1 car. A car can have at most 1 driver.)
* *Diagram Description:* The "person" entity is connected to the "drive" relationship via a double parallel line.


* **Participation Bounds:**
* *Diagram Description 1:* "person" entity connects to "own" relationship with bounds `[1..1]`. The "own" relationship connects to "car" with bounds `[0..N]`.
* *Text:* Each person can own many cars (but does not need to own one). Every car must be owned by exactly 1 person.
* *Diagram Description 2:* "staff" entity connects to "works" relationship with bounds `[3..N]`. "works" connects to "branch" with bounds `[1..1]`.
* *Text:* Every staff member must be a member of exactly 1 branch. Each branch must have at least 3 staff members.



**Advanced Entities & Relationships**

* **Weak Entity (double rectangle/diamond):** * *Diagram Description:* A double-outlined entity rectangle labeled "room" connects via a double line to a double-outlined diamond relationship labeled "is_in", which connects to a standard entity "building" (which has an underlined key attribute "name").
* *Text:* If the **strong** entity related to it is **deleted**, the **weak** entity **can't exist** (delete on cascade). The weak entity will always have **total participation** to the weak relation, and will always be the **"many"** in a 1-to-many. (Here room cannot exist without building).


* **is-a Relationships (lines with arrow):**
* *Diagram Description:* An entity "movie" (with attributes length, title, year) has an open triangle pointing towards it. Two sub-entities, "cartoon" and "western" (which has attribute "weapon"), point lines into the base of this triangle indicating inheritance. Furthermore, "actor" and "cartoon" connect to a relationship "voices", while "actor", "cartoon", and "western" all connect to a relationship "acts".
* *Text:* Cartoon/Western **inherits all attributes and relations** of movie while keeping their own extra attributes/relations. Both cartoon and western also participate in the **acts** relation, but **only** cartoon participates in the **voices** relation.



**Traps in ER Models**

* **Fan Traps:**
* *Text:* We need to be careful that the E-R model provides a workable representation of the real world and not allow ambiguous paths between entities - a **fan trap**. Consider:
* *Diagram Description:* A bipartite graph structure showing "staff" mapping to "faculty" which maps to "dept". Specific instances (Alice, Bob, Carol) map to faculties (Engineering, Science), which map to departments (Computing, EE). The text notes: "We could resolve the fan-trap (multiple paths from staff to dept) by changing the structure to:..." and shows a rearranged diagram where "faculty" maps to "dept" and "staff" maps directly to "dept", clarifying exact department membership.


* **Chasm Traps:**
* *Text:* Problems can also occur if the model suggests a relationship between entities but one doesn't exist - a **chasm trap**. Consider the following:
* *Diagram Description:* A graph mapping "dept" to "staff" to "PC". The diagram highlights a missing link where a staff member might exist without a PC mapping, causing a break in discovering which department owns which PC. The resolved diagram below it draws a direct mapping relationship between "dept" and "PC" to fix the chasm.



---

### Column 2: Relational Algebra & Functional Dependencies

**Relational Algebra**

* Selection: $\sigma_{\text{condition}}(R)$
* Projection: $\pi_{\text{attributes}}(R)$
* Product: $R \times S$
* Intersection: $R \cap S$
* Union: $R \cup S$
* Difference: $R - S$ (Note: $R - S \neq S - R$)
* Join: $R \bowtie_{\text{condition}} S$
* *Diagram Description:* Next to the formulas are tiny visual grids illustrating the operations. Selection highlights specific rows. Projection highlights specific columns. Product shows a grid multiplication. Intersection shows overlapping shaded areas. Union shows combined shaded areas. Difference shows subtraction of areas.

**Functional Dependencies**
**Armstrong's Axioms** (sound and complete) (incl. derived):

* **Reflexivity** (trivial FDs): if $b \subseteq a$ then $a \rightarrow b$
* **Augmentation:** if $a \rightarrow b$ then $ac \rightarrow bc$
* **Transitivity:** if $a \rightarrow b$ and $b \rightarrow c$ then $a \rightarrow c$
* **Union:** if $a \rightarrow b$ and $a \rightarrow c$ then $a \rightarrow bc$
* **Decomposition:** if $a \rightarrow bc$ then $a \rightarrow b$ and $a \rightarrow c$
* **Pseudotransitivity:** if $a \rightarrow b$ and $db \rightarrow c$ then $da \rightarrow c$

The **closure A+** of an **attribute A** is the set of all attributes that are either directly or indirectly dependent on A.

The **closure S+** of a set of **FDs S** is the set of all FDs that can be derived from S (ignore trivial FDs (e.g. A $\rightarrow$ A) or LHSs that are not minimal (e.g. AB $\rightarrow$ C when A $\rightarrow$ C exists)).

**Testing for extraneous attribute in an FD:**

* LHS X is extraneous if **RHS $\subseteq$ (LHS-X)+** under the FD.
* RHS X is extraneous if **X $\subseteq$ (LHS)+** under the FD set with **X removed** from the RHS.

**To compute canonical/minimal cover:**

1. Replace FDs **a $\rightarrow$ b** and **a $\rightarrow$ c** with **a $\rightarrow$ bc** (UNION RULE).
2. Remove all **extraneous** attributes one at a time.
3. Repeat until **F doesn't change**.

**Normalization**

* **BCNF:** for all non-trivial FDs (incl. derived FDs), the LHS of every FD is a superkey (i.e. contains a key); **3NF:** BCNF requirement, or every attribute on RHS of an FD is **prime** (a member of any candidate key)

**Decomposing FDs:**

* **Lossless:** If a relation R is decomposed into 2 relations R1 and R2, the decomposition is **lossless** if the common attributes of R1 and R2 form a superkey of either R1 or R2: **attr(R1) $\cap$ attr(R2) $\rightarrow$ attr(R1) or attr(R2)**
* **Dependency Preservation:** The decomposition is dependency preserving if we can check the FDs of R without joining R1 and R2. ***Note that BCNF does not always allow for this.***

**BCNF Decomposition:**
Find the **candidate keys** of the FDs and compare **LHS's** of FDs to the **keys** to find violations. Take a violating FD V: let "LHS $\rightarrow$ RHS" be this V. Split R into **R1(LHS $\cup$ RHS)** and **R2(attr(R) - RHS)**. Repeat on each decomposition until no violating FDs remain.

**3NF Decomposition:**
Find the **canonical cover C** for the set of FDs F.
For each FD (LHS $\rightarrow$ RHS) in C, add **Ri(LHS $\cup$ RHS)** to set of decomposed relations.
Remove any Ri's that are a **subset** of another.
If none of the relations in the decomposed set includes a key for R, add new **Ri(key)**.
*(Note: There is another small photo of a man's face next to this section).*

---

### Column 3: SQL Mapping & Commands

**Relational Algebra to SQL Mappings**

| RA | SQL |
| --- | --- |
| $R \cup S$ | R **UNION** S |
| $R \cap S$ | R **INTERSECT** S |
| $R - S$ | R **EXCEPT** S |
| $\pi_{\text{attributes}}(R)$ | **SELECT** attributes **FROM** R |
| $\sigma_{\text{condition}}(R)$ | **FROM** R **WHERE** condition |
| $R \times S$ | R,S *or* R **CROSS JOIN** S |
| $R \bowtie S$ | R **NATURAL JOIN** S |
| $R \bowtie_{\text{condition}} S$ | R **JOIN** S **ON** condition |

| RA | SQL |
| --- | --- |
| Relation | Table |
| Relational expression | Views |
| Tuple | Row/Record |
| Attribute | Column/Field |
| Domain | Type |

**Booleans:** Can be **True (1)**, **False (0)** or **Unknown (1/2)**.
$x \text{ AND } y = \min(x,y)$ | $x \text{ OR } y = \max(x,y)$ | $\text{NOT } x = 1 - x$
Any arithmetic involving nulls will result in a null.

**Joins** *(Note: There is a third photo of a man's face here)*

* **Inner Join** - returns tuples when there is at least one match in both sides
* **Left Outer Join** - similar to inner, but will include all tuples from the left relation
* **Right Outer Join** - similar to inner, but includes all tuples from the right relation
* **Full Outer Join** - similar to inner, but includes all unmatched tuples

**Join Example Data Table**

| L | R | L LOJ R | L ROJ R | L FOJ R |
| --- | --- | --- | --- | --- |
| **a** | **b** | **c** | **a** | **b** |
| 1 | 2 | 3 | 2 | 3 |
| 4 | 5 | 6 | 2 | 3 |
| 7 | 8 | 9 | 6 | 8 |
|  |  |  |  |  |
|  |  |  |  |  |

**E.g. using DISTINCT to remove duplicates:**

```sql
SELECT DISTINCT title, year, name FROM movie LEFT OUTER JOIN casting ON movie.producer = casting.name;

```

**Aggregate Functions:**
`COUNT(*)` **AS** profs, `SUM(salary)` **AS** totalsal, `AVG(salary)` **AS** avgsal, `MIN(age)` **AS** youngest, `MAX(age)` **AS** oldest.
*Text:* This creates a **tuple** for each department e.g. (dept="DoC", profs=5, totalsal = 420000, avgsal = 5000, youngest = 19, oldest = 84). **"AS" renames** any fields (e.g. profs would've been named COUNT(*)). Any **non-aggregates** used in the projection, or the **HAVING** filter must be included in the **GROUP BY** list. Only departments with **at least 10** professors are listed. The results are then sorted **descending** by **total salary**, and **only returns** the top **15** departments.

```sql
SELECT department AS dept, COUNT(*) AS profs, ...
FROM employee WHERE position = 'Professor' GROUP BY department HAVING COUNT(*) >= 10 ORDER BY totalsal DESC LIMIT 15;

```

**Subqueries:**

* **Scalar Subquery:** Returns a single value.
```sql
SELECT title, (SELECT COUNT(name) FROM casting WHERE casting.title = movie.title) AS numactors FROM movie;

```


* **Set Subquery:** Creates a set of distinct values (a column).
```sql
SELECT title FROM movie WHERE studio IN (SELECT name FROM studio WHERE address LIKE 'C%');

```


* **Relation Subquery:** Returns a relation (multiple columns), typically used to check if empty (`NOT EXISTS`) or contains duplicate values (`NOT UNIQUE`).
```sql
SELECT title FROM movie m1 WHERE NOT EXISTS (SELECT * FROM movie m2 WHERE m2.title = m1.title AND m1.year <> m2.year);

```



**Data Definition/Manipulation**
We can make and edit a table of the form **movie(title, year, length, genre, ISAN)** in SQL:

```sql
CREATE TABLE movie(
  title VARCHAR(120),
  year INT DEFAULT 2011,
  length INT NOT NULL,
  genre CHAR(20) NOT NULL,
  ISAN CHAR(24) NOT NULL,
  PRIMARY KEY (title, year),
  FOREIGN KEY (genre) REFERENCES genre (name) ON DELETE CASCADE,
  UNIQUE (ISAN),
  CHECK (year BETWEEN 1900 AND 2028),
  CONSTRAINT validISAN CHECK
    (ISAN IN (SELECT num FROM ISANs))
)

```

* **Varchar(120)** - string of variable length, **max length** 120.
* **Primary keys** are **(title, year),** must be **unique** and **not null** i.e. a value must be given upon input. If a user attempts to insert a record where the ISAN already exists, the insertion will **fail** since ISAN must also be **unique**.
* **Genre** is a **foreign key** referencing another table called genre: if e.g. "horror" is deleted from the genre table it will **cascade and delete** all "horror" movies from movie.
* You can add/name **check constraints** to fail for certain inputs (we named one **validISAN**).

**More SQL commands:**

* `DROP TABLE movie;`
* `ALTER TABLE movie DROP length;`
* `ALTER TABLE movie ADD studio CHAR(16) DEFAULT '';`
* `UPDATE movie SET studio = "Disney" WHERE title LIKE "Marvel%";`
* `INSERT INTO movie (title, year, genre) VALUES ('Borat', 2006, 'Comedy'), ('IT', 2017, 'Horror');`
* `DELETE FROM movie WHERE genre = "Western";`
* `CREATE INDEX yearindex ON movie (year);` Creates an **index relation** yearindex on movie using **year** as **search-key** (more on indexing later)
* `CREATE VIEW comedies AS (SELECT title, year FROM movie WHERE genre="comedy");` A **view** is a **temporary relation** which future SQL commands can call upon.

**AUTO_INCREMENT**

```sql
CREATE TABLE person(
  pID INT NOT NULL AUTO_INCREMENT,
  name VARCHAR(255)
  PRIMARY KEY (pID)
)

```

*Text:* **AUTO_INCREMENT** on a field allows for a **unique number** to be set upon record insertion.