Here is the full transcription of the image, structured logically by column and section:

### **ARRAY LEMMAS**
**Deep Equality Lemmas:**
* $a[..] \sim b[..] \longrightarrow b[..] \sim a[..]$ \hspace{1cm} ($\sim$Symm)
* $a[..] \sim b[..] \land b[..] \sim c[..] \longrightarrow a[..] \sim c[..]$ \hspace{1cm} ($\sim$Trans)
* $a[..] \sim b[..] \longrightarrow a.size = b.size$ \hspace{1cm} ($\sim$Size)
* $a[..] \sim b[..] \longrightarrow a[..] \sim b[..]$ \hspace{1cm} ($\approx$IsPrm)

**Permutation Lemmas:**
* $a[..] \sim b[..] \longrightarrow b[..] \sim a[..]$ \hspace{1cm} ($\sim$Symm)
* $a[..] \sim b[..] \land b[..] \sim c[..] \longrightarrow a[..] \sim c[..]$ \hspace{1cm} ($\sim$Trans)
* $a[..] \sim b[..] \longrightarrow a.size = b.size$ \hspace{1cm} ($\sim$Size)

**Swapped Lemmas:**
* $Swapped(a[..], b[..], i, j) \longrightarrow a[..] \approx b[..]$ \hspace{1cm} ($\approx$Swpd)
* $Swapped(a[..], b[..], i, j) \longrightarrow a[..] \sim b[..]$ \hspace{1cm} ($\sim$Swpd)

**Ranges Lemmas:**
* $b[..] \approx a[0..i) \cdot b[i..j) \cdot a[j..] \land x \le i \land j \le y$
    $\longrightarrow b[..] \approx a[0..x) \cdot b[x..y) \cdot a[y..]$ \hspace{1cm} **(RngWeak)**
* $b[..] \approx a[0..i) \cdot b[i..j) \cdot a[j..] \land a[i..j) \sim b[i..j)$
    $\longrightarrow b[..] \sim a[..]$ \hspace{1cm} **(RngPrm)**
* $a[..] \approx b[..] \land c[..] \approx c[0..i) \cdot a[i..j) \cdot c[j..)$
    $\longrightarrow c[..] \approx c[0..i) \cdot b[i..j) \cdot c[j..)$ \hspace{1cm} **(RngSwap)**

---

### **VARIABLE NOTATION**
* $x$ will always refer to the most recent value of $x$ (i.e. after the code has been executed).
* $x_{old}$ refers to the value of $x$ *before* the code is executed.
    * $Mod(\ ) = \{\}$
    * $Mod(var\ x = E) = \{\}$
    * $Mod(x = E) = \{x\}$
    * $Mod(i++) = \{i\}$
    * $Mod(i--) = \{i\}$
    * $Mod(a[k] = E) = \{a[k]\}$
    * $Mod(C_1 \ ; C_2) = Mod(C_1) \cup Mod(C_2)$
    * $Mod(if(E)\{C_1\}else\{C_2\}) = Mod(C_1) \cup Mod(C_2)$
    * $Mod(while(E)\{C\}) = Mod(C)$
* In pre-/post-/mid-conditions we use the subscript $_{-pre}$ to refer to the initial value of an input variable on entry to the function, e.g. $x_{pre}$ or $a_{pre}$

---

### **PROOF OBLIGATIONS FOR FUNCTIONS**
```
fun someFunc(x1: type, ..., xn: type): type
// PRE: P
// POST: Q
{   // MID: R
    code1
    // MID: S
    code2
    // MID: T
}
```
Then, as before, we introduce appropriate mid-conditions, such that:
* We can establish $R$ from $P$.
* If $R$ holds before the execution of `code1`, then $S$ holds after.
* If $S$ holds before the execution of `code2`, then $T$ holds after.
* We can establish $Q$ from $T$.

---

### **PROOF OBLIGATIONS**
Proving the correctness of a Hoare triple: $\{P\}$ code $\{Q\}$ is essentially a proof that $P$ modified by the **effects** of `code` implies $Q$.
For example, to prove: $\{true\}\ x = 5\ \{x > 0\}$
we would need to show that: $true \land x = 5 \longrightarrow x > 0$

---

### **PROOF OBLIGATIONS FOR VARIABLE ASSIGNMENT ($x = \dots$)**
$$\frac{P[x \mapsto x_{old}] \land x = E[x \mapsto x_{old}] \longrightarrow Q}{\{P\}\ x = E\ \{Q\}}$$

$$\frac{\{x = 5\}\ x = x + 2\ \{x = 7\}}{x_{old} = 5 \land x = x_{old} + 2 \longrightarrow x = 7}$$

---

### **PROOF OBLIGATIONS FOR MULTILINE CODE**
$$\frac{\{P\}\ code1\ \{R\} \quad \{R\}\ code2\ \{Q\}}{\{P\}\ code1 \ ; \ code2\ \{Q\}}$$

```
// PRE: a = x /\ b = y /\ c = z       (P)
c = a + b                            (M1)
// MID: a = x /\ b = y /\ c = xy
b = b * b                            (M2)
// MID: a = x /\ b = y^2 /\ c = xy
a = a * a                            (M3)
// MID: a = x^2 /\ b = y^2 /\ c = xy
c = c + c                            (M4)
// MID: a = x^2 /\ b = y^2 /\ c = 2xy
val result = a + b + c               (Q)
// POST: result = (x + y)^2
```
* **Line 2:** The pre-condition $P$ and code line 2 must establish the mid-condition $M_1$.
    * $a = x \land b = y \land c_{old} = z \land c = a + b \longrightarrow a = x \land b = y \land c = xy$
* **Line 4:** The mid-condition $M_1$ and code line 4 must establish the mid-condition $M_2$.
    * $a = x \land b_{old} = y \land c = xy \land b = b_{old} * b_{old} \longrightarrow a = x \land b = y^2 \land c = xy$
* **Line 6:** The mid-condition $M_2$ and code line 6 must establish the mid-condition $M_3$.
    * $a_{old} = x \land b = y^2 \land c = xy \land a = a_{old} * a_{old} \longrightarrow a = x^2 \land b = y^2 \land c = xy$
* **Line 8:** The mid-condition $M_3$ and code line 8 must establish the mid-condition $M_4$.
    * $a = x^2 \land b = y^2 \land c_{old} = xy \land c = c_{old} + c_{old} \longrightarrow a = x^2 \land b = y^2 \land c = 2xy$
* **Line 10:** The mid-condition $M_4$ and code line 10 must establish the post-condition $Q$.
    * $a = x^2 \land b = y^2 \land c = 2xy \land result = a + b + c \longrightarrow result = (x + y)^2$

---

### **PROOF OBLIGATIONS FOR CONDITIONALS (`if... else...`)**
$$\frac{(P \land cond)\ code1\ \{Q\} \quad (P \land \neg cond)\ code2\ \{Q\}}{\{P\}\ if (cond) \{code1\} else \{code2\}\ \{Q\}}$$

```
// PRE: true                                    (P)
var res: Int
if (x >= y) {
    // MID: x >= y                         (P /\ cond)
    res = x
    // MID: res = x /\ x >= y                    (R1)
} else {
    // MID: y > x                         (P /\ ~cond)
    res = y
    // MID: res = y /\ y > x                     (R2)
}
// MID: res = max(x, y)                           (M1)
```
**Implications mapped:**
$$P \land cond \land res = x \longrightarrow R_1$$
$$x \ge y \land res = x \longrightarrow res = x \land x \ge y$$
$$R_1 \longrightarrow M_1$$
$$res = x \land x \ge y \longrightarrow res = max(x, y)$$

$$P \land \neg cond \land res = y \longrightarrow R_2$$
$$y > x \land res = y \longrightarrow res = y \land y > x$$
$$R_2 \longrightarrow M_1$$
$$res = y \land y > x \longrightarrow res = max(x, y)$$

---

### **PROOF OBLIGATIONS FOR FUNCTION CALLS**
$$\frac{P \longrightarrow R[\bar{v} \mapsto \bar{u}] \quad P[ \bar{v} \mapsto \bar{v}_{old}] \land S[ \bar{x} \mapsto \bar{v} ][ \bar{v}_{pre} \mapsto \bar{v}_{old} ] \longrightarrow Q}{\{P\}\ someFunc(v_1, \dots, v_n)\ \{Q\}}$$

```
// MID: P
someFunc(v1, ..., vn)
// MID: Q
```
```
fun someFunc(x1: type, ..., xn: type)
// PRE: R
// POST: S
```

---

### **PROOF OBLIGATIONS FOR FUNCTION CALLS THAT RETURN**
$$\frac{P \longrightarrow R[\bar{x} \mapsto \bar{v}] \quad P[ \bar{v} \mapsto \bar{v}_{old} ][ res \mapsto res_{old}] \land S[ \bar{x} \mapsto \bar{v} ][ \bar{v}_{pre} \mapsto \bar{v}_{old} ][ res \mapsto res_{old} ] \longrightarrow Q}{\{P\}\ res = someFunc(v_1, \dots, v_n)\ \{Q\}}$$

```
// MID: P
res = someFunc(v1, ..., vn)
// MID: Q
```
```
someFunc(x1: type, ..., xn: type): type
// PRE: R
// POST: S
```

---

### **PROOF OBLIGATIONS FOR LOOPS**
$$\frac{P \longrightarrow I \quad \{I \land cond\}\ body\ \{I\} \quad I \land \neg cond \longrightarrow Q}{\{P\}\ while (cond) \{body\}\ \{Q\}}$$

Proving total correctness imposes two further proof obligations on us, namely:
* (d) $\exists v \in \mathbb{Z} . (I \land cond) \text{ body } \{V \ge v\}$
* (e) $\{I \land cond\} \text{ body } \{V_{old} > V\}$

```
// PRE: P
while (cond) {
    body
}
// POST: Q
```

---

### **PRINCIPLE OF MATHEMATICAL INDUCTION**
For any $P \subseteq \mathbb{N}$:
$$P(0) \land \forall k : \mathbb{N} . [ P(k) \longrightarrow P(k + 1) ] \longrightarrow \forall n : \mathbb{N} . P(n)$$

### **"TECHNIQUE" OF MATHEMATICAL INDUCTION**
For any $P \subseteq \mathbb{Z}$, and any $m : \mathbb{Z}$:
$$P(m) \land \forall k \ge m . [ P(k) \longrightarrow P(k + 1) ] \longrightarrow \forall n \ge m . P(n)$$

### **STRONG INDUCTION**
$$P(0) \land \forall k : \mathbb{N} . [ \forall j \in [0..k] . P(j) \longrightarrow P(k + 1) ] \longrightarrow \forall n : \mathbb{N} . P(n)$$

### **STRONG INDUCTION. for $m \in \mathbb{Z}$**
$$P(m) \land \forall k \ge m . [ \forall j \in [m..k] . P(j) \longrightarrow P(k + 1) ] \longrightarrow \forall n \ge m . P(n)$$

---

### **STRUCTURAL INDUCTION OVER LISTS**
$$P([ \ ]) \land \forall vs : [T] . \forall v : T . [ P(vs) \longrightarrow P(v : vs) ] \longrightarrow \forall xs : [T] . P(xs)$$

### **STRUCTURAL INDUCTION OVER ARBITRARY DATA STRUCTURES**
`data Nat = Zero | Succ Nat`
$$P(Zero) \land \forall n : Nat . [ P(n) \longrightarrow P(Succ \ n) ] \longrightarrow \forall n : Nat . P(n)$$

`data Tree a = Empty | Node (Tree a) a (Tree a)`
$$P(Empty) \land \forall t1, t2 : Tree \ T . \forall x : T . [ P(t1) \land P(t2) \longrightarrow P(Node \ t1 \ x \ t2) ]$$
$$\longrightarrow \forall t : Tree \ T . P(t)$$

`data BExp = Tr | Fl | BNt BExp | BAnd BExp BExp`
$$P(Tr) \land P(Fl) \land \forall b : BExp . [ P(b) \longrightarrow P(BNt \ b) ] \land$$
$$\forall b1, b2 : BExp . [ P(b1) \land P(b2) \longrightarrow P(BAnd \ b1 \ b2) ] \longrightarrow \forall b : BExp . P(b)$$

---

### **INDUCTION OVER SETS**
The set $S_{\mathbb{N}}$ is defined over the alphabet `Zero` and `Succ` through the rules
* R1 `Zero` $\in S_{\mathbb{N}}$
* R2 $\forall n \in S_{\mathbb{N}} \longrightarrow \text{Succ } n \in S_{\mathbb{N}}$

For a property $Q \subseteq S_{\mathbb{N}}$ we obtain the inductive principle:
$$Q(\text{Zero})$$
$$\land$$
$$\forall m \in S_{\mathbb{N}} . [ Q(m) \longrightarrow Q(\text{Succ } m) ]$$
$$\longrightarrow$$
$$\forall n \in S_{\mathbb{N}} . Q(n)$$

The set $OL \subset \mathbb{N}^*$ is defined through the rules
* R5 $[] \in OL$
* R6 $\forall i \in \mathbb{N} . i : [] \in OL$
* R7 $\forall i, j \in \mathbb{N}, js \in \mathbb{N}^* . [ i \le j \land j : js \in OL \longrightarrow i : j : js \in OL ]$

*(We use $[]$ for empty sequence, and $:$ for sequence concatenation.)*

For property $Q \subseteq \mathbb{N}^*$, the definition of $OL$ gives the inductive principle:
$$Q([])$$
$$\land$$
$$\forall i \in \mathbb{N} . Q(i : [])$$
$$\land$$
$$\forall i, j \in \mathbb{N}, js \in \mathbb{N}^* . [ i \le j \land Q(j : js) \longrightarrow Q(i : j : js) ]$$
$$\longrightarrow$$
$$\forall ns \in OL . Q(ns)$$

---

### **DEFINING SORTS AND SIGNATURES**
We adjust the definition of 'term' (Def. 2.1), to give each term a sort:
* each variable and constant comes with a sort $s$. To indicate which sort it is, we write $x : s$ and $c : s$. There are infinitely many variables of each sort.
* each $n$-ary function symbol $f$ comes with a template
    $$f : (s_1, \dots, s_n) \to s$$
    where $s_1, \dots, s_n,$ and $s$ are sorts.
    Note: $(s_1, \dots, s_n) \to s$ is not itself a sort.
* For such an $f$ and terms $t_1, \dots, t_n$, if $t_i$ has sort $s_i$ (for each $i$) then $f(t_1, \dots, t_n)$ is a term of sort $s$.
    Otherwise (if the $t_i$ don't all have the right sorts), $f(t_1, \dots, t_n)$ is not a term — it's just rubbish, like $|\forall \rangle \to $.
* Each $n$-ary relation symbol $R$ comes with a template
    $R(s_1, \dots, s_n)$, where $s_1, \dots, s_n$ are sorts.
    For terms $t_1, \dots, t_n$, if $t_i$ has sort $s_i$ (for each $i$) then $R(t_1, \dots, t_n)$ is a formula. Otherwise, it's rubbish.
* $t = t'$ is a formula if the terms $t, t'$ have the same sort.
    Otherwise, it's rubbish.
* Other operations ($\land, \neg, \forall, \exists$, etc) are unchanged. But it's polite to indicate the sort of a variable in $\forall, \exists$ by writing
    $$\forall x : s . \phi \quad \text{instead of just} \quad \forall x \phi$$
    $$\exists x : s . \phi \quad \text{instead of just} \quad \exists x \phi$$
    if $x$ has sort $s$. Alternatively, declare the variables of each sort.
    E.g., roughly, you can write $\forall x : \text{lecturer } \exists y : \text{pc } (\text{Bought}(x, y))$ instead of $\forall x (\text{Lecturer}(x) \to \exists y (\text{PC}(y) \land \text{Bought}(x, y)))$.

Now we can define a signature $L$ suitable for lists of type `[Nat]`.
* $L$ has constants $0, 1, \dots : \text{Nat}$, relation symbols $<, \le, >, \ge$ of sort $(\text{Nat}, \text{Nat})$, and function symbols
    * $+, -, \times : (\text{Nat}, \text{Nat}) \to \text{Nat}$
    * $[] : [\text{Nat}]$ (a constant to name the empty list)
    * $\text{cons}(\cdot) : (\text{Nat}, [\text{Nat}]) \to [\text{Nat}]$
    * $++ : ([\text{Nat}], [\text{Nat}]) \to [\text{Nat}]$
    * $\text{head} : [\text{Nat}] \to \text{Nat}$
    * $\text{tail} : [\text{Nat}] \to [\text{Nat}]$
    * $\sharp : [\text{Nat}] \to \text{Nat}$
    * $!! : ([\text{Nat}], \text{Nat}) \to \text{Nat}$
    We write the constants as $\underline{0}, \underline{1}, \dots$ to avoid confusion with actual numbers $0, 1, \dots$
* Let $x, y, z, k, n, m \dots$ be variables of sort $\text{Nat}$.
    Let $xs, ys, zs, \dots$ be variables of sort $[\text{Nat}]$.

---

### **INDUCTION OVER RELATIONS**
The predicate $Even \subseteq S_{\mathbb{N}}$
* R12 $Even(\text{Zero})$
* R13 $\forall n \in S_{\mathbb{N}} . [ Even(n) \longrightarrow Even(\text{Succ (Succ } n)) ]$

For property $Q \subseteq S_{\mathbb{N}}$, the definition of $Even$ gives the inductive principle:
$$Q(\text{Zero})$$
$$\land$$
$$\forall n \in S_{\mathbb{N}} . [ Q(n) \longrightarrow Q(\text{Succ (Succ } n)) ]$$
$$\longrightarrow$$
$$\forall n \in S_{\mathbb{N}} . [ Even(n) \longrightarrow Q(n) ]$$

---

### **INDUCTION OVER FUNCTIONS**
Given that:
* R20 $F \ 0 = 0$
* R21 $\forall j, k : \mathbb{Z} . [ j \ne 0 \land F(j - 3) = k \longrightarrow F \ j = 1 + k ]$

and a predicate $Q \subseteq \mathbb{Z} \times \mathbb{Z}$, we obtain the following inductive principle to prove that $\forall j, k : \mathbb{Z} . [ F j = k \longrightarrow Q(j, k) ]$.
$$Q(0, 0)$$
$$\land$$
$$\forall j, k : \mathbb{Z} . [ j \ne 0 \land Q(j - 3, k) \longrightarrow Q(j, 1 + k) ]$$
$$\longrightarrow$$
$$\forall j, k : \mathbb{Z} . [ F j = k \longrightarrow Q(j, k) ]$$







# Page 2
---











Here is the full transcription of the image, structured logically by column and section:

### **ARRAY LEMMAS**
**Deep Equality Lemmas:**
* $a[..] \sim b[..] \longrightarrow b[..] \sim a[..]$ \hspace{1cm} ($\sim$Symm)
* $a[..] \sim b[..] \land b[..] \sim c[..] \longrightarrow a[..] \sim c[..]$ \hspace{1cm} ($\sim$Trans)
* $a[..] \sim b[..] \longrightarrow a.size = b.size$ \hspace{1cm} ($\sim$Size)
* $a[..] \sim b[..] \longrightarrow a[..] \sim b[..]$ \hspace{1cm} ($\approx$IsPrm)

**Permutation Lemmas:**
* $a[..] \sim b[..] \longrightarrow b[..] \sim a[..]$ \hspace{1cm} ($\sim$Symm)
* $a[..] \sim b[..] \land b[..] \sim c[..] \longrightarrow a[..] \sim c[..]$ \hspace{1cm} ($\sim$Trans)
* $a[..] \sim b[..] \longrightarrow a.size = b.size$ \hspace{1cm} ($\sim$Size)

**Swapped Lemmas:**
* $Swapped(a[..], b[..], i, j) \longrightarrow a[..] \approx b[..]$ \hspace{1cm} ($\approx$Swpd)
* $Swapped(a[..], b[..], i, j) \longrightarrow a[..] \sim b[..]$ \hspace{1cm} ($\sim$Swpd)

**Ranges Lemmas:**
* $b[..] \approx a[0..i) \cdot b[i..j) \cdot a[j..] \land x \le i \land j \le y$
    $\longrightarrow b[..] \approx a[0..x) \cdot b[x..y) \cdot a[y..]$ \hspace{1cm} **(RngWeak)**
* $b[..] \approx a[0..i) \cdot b[i..j) \cdot a[j..] \land a[i..j) \sim b[i..j)$
    $\longrightarrow b[..] \sim a[..]$ \hspace{1cm} **(RngPrm)**
* $a[..] \approx b[..] \land c[..] \approx c[0..i) \cdot a[i..j) \cdot c[j..)$
    $\longrightarrow c[..] \approx c[0..i) \cdot b[i..j) \cdot c[j..)$ \hspace{1cm} **(RngSwap)**

---

### **VARIABLE NOTATION**
* $x$ will always refer to the most recent value of $x$ (i.e. after the code has been executed).
* $x_{old}$ refers to the value of $x$ *before* the code is executed.
    * $Mod(\ ) = \{\}$
    * $Mod(var\ x = E) = \{\}$
    * $Mod(x = E) = \{x\}$
    * $Mod(i++) = \{i\}$
    * $Mod(i--) = \{i\}$
    * $Mod(a[k] = E) = \{a[k]\}$
    * $Mod(C_1 \ ; C_2) = Mod(C_1) \cup Mod(C_2)$
    * $Mod(if(E)\{C_1\}else\{C_2\}) = Mod(C_1) \cup Mod(C_2)$
    * $Mod(while(E)\{C\}) = Mod(C)$
* In pre-/post-/mid-conditions we use the subscript $_{-pre}$ to refer to the initial value of an input variable on entry to the function, e.g. $x_{pre}$ or $a_{pre}$

---

### **PROOF OBLIGATIONS FOR FUNCTIONS**
```
fun someFunc(x1: type, ..., xn: type): type
// PRE: P
// POST: Q
{   // MID: R
    code1
    // MID: S
    code2
    // MID: T
}
```
Then, as before, we introduce appropriate mid-conditions, such that:
* We can establish $R$ from $P$.
* If $R$ holds before the execution of `code1`, then $S$ holds after.
* If $S$ holds before the execution of `code2`, then $T$ holds after.
* We can establish $Q$ from $T$.

---

### **PROOF OBLIGATIONS**
Proving the correctness of a Hoare triple: $\{P\}$ code $\{Q\}$ is essentially a proof that $P$ modified by the **effects** of `code` implies $Q$.
For example, to prove: $\{true\}\ x = 5\ \{x > 0\}$
we would need to show that: $true \land x = 5 \longrightarrow x > 0$

---

### **PROOF OBLIGATIONS FOR VARIABLE ASSIGNMENT ($x = \dots$)**
$$\frac{P[x \mapsto x_{old}] \land x = E[x \mapsto x_{old}] \longrightarrow Q}{\{P\}\ x = E\ \{Q\}}$$

$$\frac{\{x = 5\}\ x = x + 2\ \{x = 7\}}{x_{old} = 5 \land x = x_{old} + 2 \longrightarrow x = 7}$$

---

### **PROOF OBLIGATIONS FOR MULTILINE CODE**
$$\frac{\{P\}\ code1\ \{R\} \quad \{R\}\ code2\ \{Q\}}{\{P\}\ code1 \ ; \ code2\ \{Q\}}$$

```
// PRE: a = x /\ b = y /\ c = z       (P)
c = a + b                            (M1)
// MID: a = x /\ b = y /\ c = xy
b = b * b                            (M2)
// MID: a = x /\ b = y^2 /\ c = xy
a = a * a                            (M3)
// MID: a = x^2 /\ b = y^2 /\ c = xy
c = c + c                            (M4)
// MID: a = x^2 /\ b = y^2 /\ c = 2xy
val result = a + b + c               (Q)
// POST: result = (x + y)^2
```
* **Line 2:** The pre-condition $P$ and code line 2 must establish the mid-condition $M_1$.
    * $a = x \land b = y \land c_{old} = z \land c = a + b \longrightarrow a = x \land b = y \land c = xy$
* **Line 4:** The mid-condition $M_1$ and code line 4 must establish the mid-condition $M_2$.
    * $a = x \land b_{old} = y \land c = xy \land b = b_{old} * b_{old} \longrightarrow a = x \land b = y^2 \land c = xy$
* **Line 6:** The mid-condition $M_2$ and code line 6 must establish the mid-condition $M_3$.
    * $a_{old} = x \land b = y^2 \land c = xy \land a = a_{old} * a_{old} \longrightarrow a = x^2 \land b = y^2 \land c = xy$
* **Line 8:** The mid-condition $M_3$ and code line 8 must establish the mid-condition $M_4$.
    * $a = x^2 \land b = y^2 \land c_{old} = xy \land c = c_{old} + c_{old} \longrightarrow a = x^2 \land b = y^2 \land c = 2xy$
* **Line 10:** The mid-condition $M_4$ and code line 10 must establish the post-condition $Q$.
    * $a = x^2 \land b = y^2 \land c = 2xy \land result = a + b + c \longrightarrow result = (x + y)^2$

---

### **PROOF OBLIGATIONS FOR CONDITIONALS (`if... else...`)**
$$\frac{(P \land cond)\ code1\ \{Q\} \quad (P \land \neg cond)\ code2\ \{Q\}}{\{P\}\ if (cond) \{code1\} else \{code2\}\ \{Q\}}$$

```
// PRE: true                                    (P)
var res: Int
if (x >= y) {
    // MID: x >= y                         (P /\ cond)
    res = x
    // MID: res = x /\ x >= y                    (R1)
} else {
    // MID: y > x                         (P /\ ~cond)
    res = y
    // MID: res = y /\ y > x                     (R2)
}
// MID: res = max(x, y)                           (M1)
```
**Implications mapped:**
$$P \land cond \land res = x \longrightarrow R_1$$
$$x \ge y \land res = x \longrightarrow res = x \land x \ge y$$
$$R_1 \longrightarrow M_1$$
$$res = x \land x \ge y \longrightarrow res = max(x, y)$$

$$P \land \neg cond \land res = y \longrightarrow R_2$$
$$y > x \land res = y \longrightarrow res = y \land y > x$$
$$R_2 \longrightarrow M_1$$
$$res = y \land y > x \longrightarrow res = max(x, y)$$

---

### **PROOF OBLIGATIONS FOR FUNCTION CALLS**
$$\frac{P \longrightarrow R[\bar{v} \mapsto \bar{u}] \quad P[ \bar{v} \mapsto \bar{v}_{old}] \land S[ \bar{x} \mapsto \bar{v} ][ \bar{v}_{pre} \mapsto \bar{v}_{old} ] \longrightarrow Q}{\{P\}\ someFunc(v_1, \dots, v_n)\ \{Q\}}$$

```
// MID: P
someFunc(v1, ..., vn)
// MID: Q
```
```
fun someFunc(x1: type, ..., xn: type)
// PRE: R
// POST: S
```

---

### **PROOF OBLIGATIONS FOR FUNCTION CALLS THAT RETURN**
$$\frac{P \longrightarrow R[\bar{x} \mapsto \bar{v}] \quad P[ \bar{v} \mapsto \bar{v}_{old} ][ res \mapsto res_{old}] \land S[ \bar{x} \mapsto \bar{v} ][ \bar{v}_{pre} \mapsto \bar{v}_{old} ][ res \mapsto res_{old} ] \longrightarrow Q}{\{P\}\ res = someFunc(v_1, \dots, v_n)\ \{Q\}}$$

```
// MID: P
res = someFunc(v1, ..., vn)
// MID: Q
```
```
someFunc(x1: type, ..., xn: type): type
// PRE: R
// POST: S
```

---

### **PROOF OBLIGATIONS FOR LOOPS**
$$\frac{P \longrightarrow I \quad \{I \land cond\}\ body\ \{I\} \quad I \land \neg cond \longrightarrow Q}{\{P\}\ while (cond) \{body\}\ \{Q\}}$$

Proving total correctness imposes two further proof obligations on us, namely:
* (d) $\exists v \in \mathbb{Z} . (I \land cond) \text{ body } \{V \ge v\}$
* (e) $\{I \land cond\} \text{ body } \{V_{old} > V\}$

```
// PRE: P
while (cond) {
    body
}
// POST: Q
```

---

### **PRINCIPLE OF MATHEMATICAL INDUCTION**
For any $P \subseteq \mathbb{N}$:
$$P(0) \land \forall k : \mathbb{N} . [ P(k) \longrightarrow P(k + 1) ] \longrightarrow \forall n : \mathbb{N} . P(n)$$

### **"TECHNIQUE" OF MATHEMATICAL INDUCTION**
For any $P \subseteq \mathbb{Z}$, and any $m : \mathbb{Z}$:
$$P(m) \land \forall k \ge m . [ P(k) \longrightarrow P(k + 1) ] \longrightarrow \forall n \ge m . P(n)$$

### **STRONG INDUCTION**
$$P(0) \land \forall k : \mathbb{N} . [ \forall j \in [0..k] . P(j) \longrightarrow P(k + 1) ] \longrightarrow \forall n : \mathbb{N} . P(n)$$

### **STRONG INDUCTION. for $m \in \mathbb{Z}$**
$$P(m) \land \forall k \ge m . [ \forall j \in [m..k] . P(j) \longrightarrow P(k + 1) ] \longrightarrow \forall n \ge m . P(n)$$

---

### **STRUCTURAL INDUCTION OVER LISTS**
$$P([ \ ]) \land \forall vs : [T] . \forall v : T . [ P(vs) \longrightarrow P(v : vs) ] \longrightarrow \forall xs : [T] . P(xs)$$

### **STRUCTURAL INDUCTION OVER ARBITRARY DATA STRUCTURES**
`data Nat = Zero | Succ Nat`
$$P(Zero) \land \forall n : Nat . [ P(n) \longrightarrow P(Succ \ n) ] \longrightarrow \forall n : Nat . P(n)$$

`data Tree a = Empty | Node (Tree a) a (Tree a)`
$$P(Empty) \land \forall t1, t2 : Tree \ T . \forall x : T . [ P(t1) \land P(t2) \longrightarrow P(Node \ t1 \ x \ t2) ]$$
$$\longrightarrow \forall t : Tree \ T . P(t)$$

`data BExp = Tr | Fl | BNt BExp | BAnd BExp BExp`
$$P(Tr) \land P(Fl) \land \forall b : BExp . [ P(b) \longrightarrow P(BNt \ b) ] \land$$
$$\forall b1, b2 : BExp . [ P(b1) \land P(b2) \longrightarrow P(BAnd \ b1 \ b2) ] \longrightarrow \forall b : BExp . P(b)$$

---

### **INDUCTION OVER SETS**
The set $S_{\mathbb{N}}$ is defined over the alphabet `Zero` and `Succ` through the rules
* R1 `Zero` $\in S_{\mathbb{N}}$
* R2 $\forall n \in S_{\mathbb{N}} \longrightarrow \text{Succ } n \in S_{\mathbb{N}}$

For a property $Q \subseteq S_{\mathbb{N}}$ we obtain the inductive principle:
$$Q(\text{Zero})$$
$$\land$$
$$\forall m \in S_{\mathbb{N}} . [ Q(m) \longrightarrow Q(\text{Succ } m) ]$$
$$\longrightarrow$$
$$\forall n \in S_{\mathbb{N}} . Q(n)$$

The set $OL \subset \mathbb{N}^*$ is defined through the rules
* R5 $[] \in OL$
* R6 $\forall i \in \mathbb{N} . i : [] \in OL$
* R7 $\forall i, j \in \mathbb{N}, js \in \mathbb{N}^* . [ i \le j \land j : js \in OL \longrightarrow i : j : js \in OL ]$

*(We use $[]$ for empty sequence, and $:$ for sequence concatenation.)*

For property $Q \subseteq \mathbb{N}^*$, the definition of $OL$ gives the inductive principle:
$$Q([])$$
$$\land$$
$$\forall i \in \mathbb{N} . Q(i : [])$$
$$\land$$
$$\forall i, j \in \mathbb{N}, js \in \mathbb{N}^* . [ i \le j \land Q(j : js) \longrightarrow Q(i : j : js) ]$$
$$\longrightarrow$$
$$\forall ns \in OL . Q(ns)$$

---

### **DEFINING SORTS AND SIGNATURES**
We adjust the definition of 'term' (Def. 2.1), to give each term a sort:
* each variable and constant comes with a sort $s$. To indicate which sort it is, we write $x : s$ and $c : s$. There are infinitely many variables of each sort.
* each $n$-ary function symbol $f$ comes with a template
    $$f : (s_1, \dots, s_n) \to s$$
    where $s_1, \dots, s_n,$ and $s$ are sorts.
    Note: $(s_1, \dots, s_n) \to s$ is not itself a sort.
* For such an $f$ and terms $t_1, \dots, t_n$, if $t_i$ has sort $s_i$ (for each $i$) then $f(t_1, \dots, t_n)$ is a term of sort $s$.
    Otherwise (if the $t_i$ don't all have the right sorts), $f(t_1, \dots, t_n)$ is not a term — it's just rubbish, like $|\forall \rangle \to $.
* Each $n$-ary relation symbol $R$ comes with a template
    $R(s_1, \dots, s_n)$, where $s_1, \dots, s_n$ are sorts.
    For terms $t_1, \dots, t_n$, if $t_i$ has sort $s_i$ (for each $i$) then $R(t_1, \dots, t_n)$ is a formula. Otherwise, it's rubbish.
* $t = t'$ is a formula if the terms $t, t'$ have the same sort.
    Otherwise, it's rubbish.
* Other operations ($\land, \neg, \forall, \exists$, etc) are unchanged. But it's polite to indicate the sort of a variable in $\forall, \exists$ by writing
    $$\forall x : s . \phi \quad \text{instead of just} \quad \forall x \phi$$
    $$\exists x : s . \phi \quad \text{instead of just} \quad \exists x \phi$$
    if $x$ has sort $s$. Alternatively, declare the variables of each sort.
    E.g., roughly, you can write $\forall x : \text{lecturer } \exists y : \text{pc } (\text{Bought}(x, y))$ instead of $\forall x (\text{Lecturer}(x) \to \exists y (\text{PC}(y) \land \text{Bought}(x, y)))$.

Now we can define a signature $L$ suitable for lists of type `[Nat]`.
* $L$ has constants $0, 1, \dots : \text{Nat}$, relation symbols $<, \le, >, \ge$ of sort $(\text{Nat}, \text{Nat})$, and function symbols
    * $+, -, \times : (\text{Nat}, \text{Nat}) \to \text{Nat}$
    * $[] : [\text{Nat}]$ (a constant to name the empty list)
    * $\text{cons}(\cdot) : (\text{Nat}, [\text{Nat}]) \to [\text{Nat}]$
    * $++ : ([\text{Nat}], [\text{Nat}]) \to [\text{Nat}]$
    * $\text{head} : [\text{Nat}] \to \text{Nat}$
    * $\text{tail} : [\text{Nat}] \to [\text{Nat}]$
    * $\sharp : [\text{Nat}] \to \text{Nat}$
    * $!! : ([\text{Nat}], \text{Nat}) \to \text{Nat}$
    We write the constants as $\underline{0}, \underline{1}, \dots$ to avoid confusion with actual numbers $0, 1, \dots$
* Let $x, y, z, k, n, m \dots$ be variables of sort $\text{Nat}$.
    Let $xs, ys, zs, \dots$ be variables of sort $[\text{Nat}]$.

---

### **INDUCTION OVER RELATIONS**
The predicate $Even \subseteq S_{\mathbb{N}}$
* R12 $Even(\text{Zero})$
* R13 $\forall n \in S_{\mathbb{N}} . [ Even(n) \longrightarrow Even(\text{Succ (Succ } n)) ]$

For property $Q \subseteq S_{\mathbb{N}}$, the definition of $Even$ gives the inductive principle:
$$Q(\text{Zero})$$
$$\land$$
$$\forall n \in S_{\mathbb{N}} . [ Q(n) \longrightarrow Q(\text{Succ (Succ } n)) ]$$
$$\longrightarrow$$
$$\forall n \in S_{\mathbb{N}} . [ Even(n) \longrightarrow Q(n) ]$$

---

### **INDUCTION OVER FUNCTIONS**
Given that:
* R20 $F \ 0 = 0$
* R21 $\forall j, k : \mathbb{Z} . [ j \ne 0 \land F(j - 3) = k \longrightarrow F \ j = 1 + k ]$

and a predicate $Q \subseteq \mathbb{Z} \times \mathbb{Z}$, we obtain the following inductive principle to prove that $\forall j, k : \mathbb{Z} . [ F j = k \longrightarrow Q(j, k) ]$.
$$Q(0, 0)$$
$$\land$$
$$\forall j, k : \mathbb{Z} . [ j \ne 0 \land Q(j - 3, k) \longrightarrow Q(j, 1 + k) ]$$
$$\longrightarrow$$
$$\forall j, k : \mathbb{Z} . [ F j = k \longrightarrow Q(j, k) ]$$