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









Here is the full transcription of the second page of the cheat sheet, structured logically by column and section:

### **Definition 1.1 (Propositional formula)**
Any propositional atom ($p, q, r$, etc) is a propositional formula.
$\top$ and $\bot$ are formulas.
If $\phi$ is a formula then so is $(\neg \phi)$. *Note the brackets!*
If $\phi, \psi$ are formulas then so are $(\phi \land \psi)$, $(\phi \lor \psi)$, $(\phi \rightarrow \psi)$, and $(\phi \leftrightarrow \psi)$.
That's it: nothing is a formula unless built by these rules.
(strongest) $\neg, \land, \lor, \rightarrow, \leftrightarrow$ (weakest)

For example, $\neg p \lor q \rightarrow (p \leftrightarrow q \land r)$ has the formation tree:
[Tree diagram showing root $\rightarrow$ branching to $\lor$ and $\leftrightarrow$, etc.]
Note that the connective at the root (top!) of the tree is $\rightarrow$. This is the *principal connective* or *main connective* of $\neg p \lor q \rightarrow (p \leftrightarrow q \land r)$.
This formula has the overall *logical form* $\phi \rightarrow \psi$.
The subformulas of $\neg p \lor q \rightarrow (p \leftrightarrow q \land r)$ are:
$\neg p \lor q \rightarrow (p \leftrightarrow q \land r)$
$\neg p \lor q$ \hspace{1cm} $p \leftrightarrow q \land r$
$\neg p$ \hspace{1cm} $q$ \hspace{1cm} $p$ \hspace{1cm} $q \land r$
$p$ \hspace{3.5cm} $q$ \hspace{1cm} $r$

### **Definition 1.2**
* A formula of the form $\top, \bot$ or $p$ for an atom $p$, is (as we know) called *atomic*.
* A formula whose logical form is $\neg \phi$ is called a *negated formula*. A formula of the form $\neg p, \neg \top$, or $\neg \bot$ is sometimes called *negated-atomic*.
* A formula of the form $\phi \land \psi$ is called a *conjunction* and $\phi, \psi$ are its *conjuncts*.
* A formula of the form $\phi \lor \psi$ is called a *disjunction*, and $\phi, \psi$ are its *disjuncts*.
* A formula of the form $\phi \rightarrow \psi$ is called an *implication*. $\phi$ is called the *antecedent*, $\psi$ is called the *consequent*.
* A formula of the form $\phi \leftrightarrow \psi$ is called a *biconditional implication*.

### **Definition 1.3 (Literals and clauses)**
A formula that is either atomic or negated-atomic is called a *literal*.
A *clause* is a disjunction ($\lor$) of one or more literals.

### **Definition 1.4 (Atomic evaluation function)**
Let $A$ be a set of propositional atoms. An *atomic evaluation function* $v : A \rightarrow \{tt, ff\}$ assigns truth-values to each propositional atom in $A$.

### **Definition 1.5 (Evaluation function)**
Let $A$ be a set of propositional atoms, and $v$ an atomic evaluation function for $A$. The *evaluation function* $[\dots]_v$ assigns the truth-value *true* ($tt$) or *false* ($ff$) to formulas as follows.
* If $\phi$ is an atom $p \in A$: $[p]_v = tt$ iff $v(p) = tt$
* $[\neg \phi]_v = tt$ iff $[\phi]_v = ff$
* $[\phi \land \psi]_v = tt$ iff $[\phi]_v = tt$ and $[\psi]_v = tt$
* $[\phi \lor \psi]_v = tt$ iff $[\phi]_v = tt$ or $[\psi]_v = tt$
* $[\phi \rightarrow \psi]_v = tt$ iff $[\phi]_v = ff$ or $[\psi]_v = tt$
* $[\phi \leftrightarrow \psi]_v = tt$ iff $[\phi]_v = [\psi]_v$
* $[\bot]_v = ff$ \hspace{1cm} $[\top]_v = tt$

### **Definition 1.6 (Functional completeness)**
Let $C$ be a set of Boolean connectives. $C$ is functionally complete (for propositional logic) if any connective of any arity can be defined just in terms of the connectives in $C$.

* 'But' means 'and'. (So does *yet, although, though*.)
  E.g., **I will go out, but it is raining**
  Let $g$ be an atom for 'I will go out' and $r$ for 'it is raining'. We get $(g \land r)$.
* 'Unless' generally means 'or'.
  E.g., **I will go out unless it rains**
  Let $g$ be an atom for 'I will go out' and $w$ for 'it will rain'. (Note the extra 'will')
  We get $(g \lor w)$. You could also use $((\neg w) \rightarrow g)$.

But you may think that 'I will go out unless it rains' implies that if it does rain then I won't go out. This is a *stronger* reading of 'unless'.
* *Strong 'Unless'* (also called "exclusive or").
  'I will go out unless it rains' becomes $(g \leftrightarrow (\neg w))$.

---

### **English to Logic Variants**

* 'Only if'
  You will pass only if your average is at least forty $\rightarrow$ ('You will pass' $\rightarrow$ 'your average is at least forty')
* 'Is necessary for'
  James' attending the course is necessary for his obtaining a certificate of attendance $\rightarrow$ ('James obtains a certificate of attendance' $\rightarrow$ 'James attends the course')
* 'Is sufficient for'
  Layla's timely arrival for rehearsals is sufficient for her taking part in the play $\rightarrow$ ('Layla arrives on time for rehearsals' $\rightarrow$ 'Layla will take part in the play')

Other variants: *provided that, on the condition that, in case, ...*

### **Definition 1.7 (valid argument)**
Given formulas $\phi_1, \phi_2, \dots, \phi_n, \psi$, an argument
$$\phi_1, \phi_2, \dots, \phi_n \text{, therefore } \psi$$
is *valid* if:
$\psi$ is true in every situation in which $\phi_1, \phi_2, \dots, \phi_n$ are all true.
If this is so, we write '$\phi_1, \phi_2, \dots, \phi_n \models \psi$'.

### **Definition 1.8 (valid formula)**
A propositional formula is *(logically) valid* if it is true in every situation.

### **Definition 1.9 (satisfiable formula)**
A propositional formula is *satisfiable* if it is true in at least one situation.

### **Definition 1.10 (equivalent formulas)**
Two propositional formulas $\phi, \psi$ are *logically equivalent* if they are true in exactly the same situations. Roughly speaking: they mean the same.

| argument validity | formula validity | satisfiability | equivalence |
| :--- | :--- | :--- | :--- |
| $\phi \models \psi$ | $\models \phi \rightarrow \psi \text{ valid}$ | $\phi \land \neg \psi \text{ unsatisfiable}$ | $(\phi \rightarrow \psi) \equiv \top$ |
| $\top \models \phi$ | $\models \phi \text{ valid}$ | $\neg \phi \text{ unsatisfiable}$ | $\phi \equiv \top$ |
| $\phi \not\models \bot$ | $\neg \phi \text{ not valid}$ | $\phi \text{ satisfiable}$ | $\phi \not\equiv \bot$ |
| $\phi \models \psi \text{ and } \psi \models \phi$ | $\models \phi \leftrightarrow \psi \text{ valid}$ | $\phi \leftrightarrow \neg \psi \text{ unsatisfiable}$ | $\phi \equiv \psi$ |

### **Definition 1.11**
Let $\phi_1, \dots, \phi_n \models \psi$ be an argument. We call the formula $\phi_1 \land \dots \land \phi_n \rightarrow \psi$ its *corresponding implication formula*.

### **Theorem 1.12**
$\phi_1, \dots, \phi_n \models \psi$ is a valid argument if and only if its corresponding implication formula $\phi_1 \land \dots \land \phi_n \rightarrow \psi$ is a valid formula (i.e., tautology).

### **Logical Equivalences**
**$\land$**
1. $\phi \land \psi \text{ is logically equivalent to } \psi \land \phi$ (commutativity of $\land$)
2. $\phi \land \phi \text{ is logically equivalent to } \phi$ (idempotence of $\land$)
3. $\phi \land \top \text{ and } \top \land \phi \text{ are logically equivalent to } \phi$
4. $\bot \land \phi \text{, } \phi \land \bot \text{, } \phi \land \neg \phi \text{, and } \neg \phi \land \phi \text{ are all equivalent to } \bot$
5. $(\phi \land \psi) \land \rho \text{ is equivalent to } \phi \land (\psi \land \rho)$ (associativity of $\land$)

**$\lor$**
6. $\phi \lor \psi \text{ is equivalent to } \psi \lor \phi$ (commutativity of $\lor$)
7. $\phi \lor \phi \text{ is equivalent to } \phi$ (idempotence of $\lor$)
8. $\top \lor \phi \text{, } \phi \lor \top \text{, } \phi \lor \neg \phi \text{, and } \neg \phi \lor \phi \text{ are equivalent to } \top$
9. $\phi \lor \bot \text{ and } \bot \lor \phi \text{ are equivalent to } \phi$
10. $(\phi \lor \psi) \lor \rho \text{ is equivalent to } \phi \lor (\psi \lor \rho)$ (associativity of $\lor$)

**$\neg$**
11. $\neg \top \text{ is equivalent to } \bot$
12. $\neg \bot \text{ is equivalent to } \top$
13. $\neg \neg \phi \text{ is equivalent to } \phi$

**$\rightarrow$**
14. $\phi \rightarrow \top \text{ is equivalent to } \top$
15. $\top \rightarrow \phi \text{ is equivalent to } \phi$
16. $\phi \rightarrow \top \text{ is equivalent to } \top$ *(Note: Duplicate in image)*
17. $\bot \rightarrow \phi \text{ is equivalent to } \top$
18. $\phi \rightarrow \bot \text{ is equivalent to } \neg \phi$
19. $\phi \rightarrow \psi \text{ is equivalent to } \neg \phi \lor \psi \text{, and also to } \neg (\phi \land \neg \psi)$
20. $\neg (\phi \rightarrow \psi) \text{ is equivalent to } \phi \land \neg \psi$

**$\leftrightarrow$**
21. $\phi \leftrightarrow \psi \text{ is equivalent to } (\phi \rightarrow \psi) \land (\psi \rightarrow \phi) \text{, } (\phi \land \psi) \lor (\neg \phi \land \neg \psi) \text{, } \neg (\phi \leftrightarrow \neg \psi)$
22. $\neg (\phi \leftrightarrow \psi) \text{ is equivalent to } \phi \leftrightarrow \neg \psi \text{, } \neg \phi \leftrightarrow \psi \text{, } (\phi \land \neg \psi) \lor (\neg \phi \land \psi)$
23. $\neg (\phi \land \psi) \text{ is equivalent to } \neg \phi \lor \neg \psi$
24. $\neg (\phi \lor \psi) \text{ is equivalent to } \neg \phi \land \neg \psi$
25. $\phi \land (\psi \lor \rho) \text{ is equivalent to } (\phi \land \psi) \lor (\phi \land \rho)$. $(\phi \lor \rho) \land \psi \text{ is equivalent to } (\phi \land \psi) \lor (\rho \land \psi)$.
26. $\phi \lor (\psi \land \rho) \text{ is equivalent to } (\phi \lor \psi) \land (\phi \lor \rho)$. $(\phi \land \rho) \lor \psi \text{ is equivalent to } (\phi \lor \psi) \land (\rho \lor \psi)$.
27. $\phi \land (\phi \lor \psi) \text{ and } \phi \lor (\phi \land \psi) \text{ are equivalent to } \phi$. So are $\phi \land (\psi \lor \phi)$, $(\phi \land \psi) \lor \phi$, etc.

---

### **Definition 1.13 (Normal form DNF)**
A formula $\phi$ is in *disjunctive normal form* if it is a disjunction of conjunctions of literals, and is not further simplifiable by equivalences without leaving this form. (See Def. 1.3 for literals.)

### **Definition 1.14 (Normal form CNF)**
A formula $\phi$ is in *conjunctive normal form* if it is a conjunction of disjunctions of literals (that is, a conjunction of clauses), and is not further simplifiable by equivalences without leaving this form.

1. Remove all occurrences of $\rightarrow$ and $\leftrightarrow$ by
   - replacing all subformulas $\phi \rightarrow \psi$ by $\neg \phi \lor \psi$
   - replacing all subformulas $\phi \leftrightarrow \psi$ by $(\phi \land \psi) \lor (\neg \phi \land \neg \psi)$
   It's faster to replace $\neg(\phi \rightarrow \psi)$ by $\phi \land \neg \psi$, and $\neg(\phi \leftrightarrow \psi)$ by $(\phi \land \neg \psi) \lor (\neg \phi \land \psi)$.
2. Use the De Morgan laws to push negations down next to atoms. Delete all double negations (replace $\neg \neg \phi$ by $\phi$).
3. Rearrange using distributivity to get the desired normal form.
4. Simplify:
   - replacing subformulas $p \land \neg p$ by $\bot$, and $p \lor \neg p$ by $\top$.
   - replacing subformulas $\top \lor \rho$ by $\top$, $\top \land \rho$ by $\rho$, $\bot \lor \rho$ by $\rho$, and $\bot \land \rho$ by $\bot$.
   - absorption (equivalence 27) is often useful too.
   - repeat till no further progress.

### **Natural Deduction Rules**

**$\land$ Rules**
**$\land$ I**
1 $\phi$
2 $\psi$
3 $\phi \land \psi \quad \land I(1,2)$

**$\land$ E**
1 $\phi \land \psi$
2 $\phi \quad \land E(1)$
3 $\psi \quad \land E(1)$

**$\rightarrow$ Rules**
**$\rightarrow$ I**
1 $|$ $\phi$ \hspace{2cm} asm
2 $|$ $\vdots$ \hspace{1cm} (the proof) hard struggle
3 $|$ $\psi$ \hspace{2cm} we made it!
4 $\phi \rightarrow \psi \quad \rightarrow I(1,3)$

**$\rightarrow$ E**
1 $\phi \rightarrow \psi$
2 $\phi$
3 $\psi \quad \rightarrow E(1,2)$

**$\lor$ Rules**
**$\lor$ I**
1 $\phi$
2 $\phi \lor \psi \quad \lor I(1)$
3 $\psi \lor \phi \quad \lor I(1)$

**$\lor$ E**
1 $\phi \lor \psi$
2 $|$ $\phi$ \hspace{1cm} asm
$\vdots$ $|$ $\vdots$
6 $|$ $\rho$
7 $|$ $\psi$ \hspace{1cm} asm
$\vdots$ $|$ $\vdots$
9 $|$ $\rho$
10 $\rho \quad \lor E(1, 2-6, 7-9)$

**$\neg$ Rules**
**$\neg$ I**
1 $|$ $\phi$ \hspace{1cm} asm
2 $|$ $\vdots$
3 $|$ $\bot$
4 $\neg \phi \quad \neg I(1,3)$

**$\neg$ E**
1 $\phi$
2 $\neg \phi$
3 $\bot \quad \neg E(1,2)$

**$\bot$ Rules**
**$\bot$ I**
1 $\phi$
2 $\neg \phi$
3 $\bot \quad \bot I(1,2)$

**$\bot$ E**
1 $\bot$ \hspace{1cm} got this somehow
2 $\vdots$
3 $\neg \phi$ \hspace{1cm} and this
4 $\bot \quad \bot I(1,3)$  *(Note: this appears to be a typo in the original image for $\bot E$, representing ex falso quodlibet: 1 $\bot$, 2 $\phi$ $\bot E(1)$)*

**$\neg \neg$ Rules**
**$\neg \neg$ E**
1 $\neg \neg \phi$
2 $\phi \quad \neg \neg E(1)$

**$\leftrightarrow$ Rules**
**$\leftrightarrow$ I**
1 $\phi \rightarrow \psi$
2 $\psi \rightarrow \phi$
3 $\phi \leftrightarrow \psi \quad \leftrightarrow I(1,2)$

**$\leftrightarrow$ E**
1 $\phi \leftrightarrow \psi$
2 $\phi \rightarrow \psi \quad \leftrightarrow E(1)$
3 $\psi \rightarrow \phi \quad \leftrightarrow E(1)$

### **Definition 1.15 (Natural deduction proof)**
Let $\phi_1, \dots, \phi_n, \psi$ be arbitrary formulas.
$$\phi_1, \dots, \phi_n \vdash \psi$$
means that there is a (natural deduction) proof of $\psi$, starting with the formulas $\phi_1, \dots, \phi_n$ as premises.

### **Definition 1.16 (Soundness and completeness)**
A proof system is *sound* if every theorem is valid, and *complete* if every valid formula is a theorem.

### **Theorem 1.17 (Soundness of natural deduction)**
Let $\phi_1, \dots, \phi_n, \psi$ be any propositional formulas. If $\phi_1, \dots, \phi_n \vdash \psi$, then $\phi_1, \dots, \phi_n \models \psi$.

### **Theorem 1.18 (Completeness)**
Let $\phi_1, \dots, \phi_n, \psi$ be any propositional formulas. If $\phi_1, \dots, \phi_n \models \psi$, then $\phi_1, \dots, \phi_n \vdash \psi$.

### **Definition 1.19 (Consistency)**
A formula $\phi$ is said to be *consistent* if $\not\vdash \neg \phi$.
A collection $\phi_1, \dots, \phi_n$ of formulas is said to be consistent if $\not\vdash \neg(\bigwedge_{i=1}^{n} \phi_i)$.

### **Theorem 1.20**
A formula $\phi$ is consistent if and only if it is satisfiable.

### **Definition 1.21 (Provable equivalence)**
Two propositional formulas $\phi$ and $\psi$ are *provably equivalent* if and only if $\vdash \phi \leftrightarrow \psi$, denoted $\phi \dashv \vdash \psi$.

### **Theorem 1.22**
Two formulas are *provably equivalent* if and only if they are *semantically equivalent*.

---

### **First-Order Logic Syntactically**
Syntactically, there are *6 new features*:
1. **Predicates** (that take *arguments*): `Sister(bob, mary)`, $\dots$
2. **Constants**: `bob, room_308`, $\dots$
3. **Variables**: `x, y, z`, $\dots$
4. **Quantifiers**: $\forall$ (for all); $\exists$ (there exists)
5. **Functions**: `father_of(_)`, `sum_of(_, _)`, `+`, `-`, `\times`, $\dots$
6. **Equality**: $=$

### **Definition 2.1 (term)**
For a signature $L$.
1. Any constant in $L$ is an $L$-term.
2. Any variable is an $L$-term.
3. If $f$ is an $n$-ary function symbol in $L$, and $t_1, \dots, t_n$ are $L$-terms, then $f(t_1, \dots, t_n)$ is an $L$-term.
4. Nothing else is an $L$-term.

A *closed term* or (as computing people say) *ground term* is one that doesn't involve a variable.

### **Definition 2.2 (formula)**
Fix a signature $L$.
1. If $R$ is an $n$-ary predicate symbol in $L$, and $t_1, \dots, t_n$ are $L$-terms, then $R(t_1, \dots, t_n)$ is an atomic $L$-formula.
2. If $t, t'$ are $L$-terms then $t = t'$ is an atomic $L$-formula. (Equality — very useful!)
3. $\top, \bot$ are atomic $L$-formulas.
4. If $\phi, \psi$ are $L$-formulas then so are $(\neg \phi), (\phi \land \psi), (\phi \lor \psi), (\phi \rightarrow \psi),$ and $(\phi \leftrightarrow \psi)$.
5. If $\phi$ is an $L$-formula and $x$ a variable, then $(\forall x \ \phi)$ and $(\exists x \ \phi)$ are $L$-formulas.
6. Nothing else is an $L$-formula.

* An *atomic formula* is a predicate symbol with arguments filled in with terms.
  * `Lecturer(susan)`
  * `PC(x)`
* A *literal* is an atomic formula or its negation:
  * `Sum_of(x, 4) = 10`
  * $\neg$`Lecturer(mother_of(x))`
* A *sentence* is a formula with all variables in the *scope* of a quantifier.
  * $\forall x \forall y$`Bought(x, y)`

### **Definition 2.3 ($L$-structure)**
Let $L$ be a signature $\langle \mathcal{K}, \mathcal{F}, \mathcal{P} \rangle$. An *$L$-structure* (or sometimes (loosely) a *model*) $M$ is a pair: $M = (D, I)$, where
* $D$ is a *domain of discourse* of $M$, a non-empty set of objects that $M$ 'knows about'. It's also called *universe* of $M$, and sometimes written as $dom(M)$.
* $I$ is an *interpretation* that specifies the meaning of each symbol in $L$ in terms of the objects in $D$:
  * for each constant $c$ in $\mathcal{K}$, $I(c) = c_M \in D$
  * for each $n$-ary function symbol $f$ in $\mathcal{F}$, $I(f) = f_M : D^n \rightarrow D$ for $n > 0$.
  * for each $n$-ary predicate symbol $P$ in $\mathcal{P}$, $I(P) = P_M \subseteq D^n$ for $n \ge 0$.

### **Definition 2.4 (free and bound variables)**
Let $\phi$ be a formula.
1. An occurrence of a variable $x$ in $\phi$ is said to be *bound* if it occurs in the *scope of a quantifier* $\forall x$ or $\exists x$.
2. Variables that are not bound are said to be *free*.
3. The *free variables of $\phi$* are those variables with free occurrences in $\phi$.

### **Definition 2.5 (assignment)**
Let $M = (D, I)$ be a structure. An *assignment* (or *'valuation'*) *over $M$* is a function that assigns an object in $D$ to each variable. That is, $h : V \rightarrow D$ is an assignment, where $V$ is the set of variables.
For an assignment $h$ and a variable $x$, we write $h(x)$ to denote the object in $D$ assigned to $x$ by $h$.

### **Definition 2.6 (value of a term)**
Let $L$ be a signature, $M = (D, I)$ an $L$-structure, and $h$ an assignment over $M$. Then for any $L$-term $t$, the *value of $t$ in $M$ under $h$*, denoted as $[t]^h_M$, is the object in $D$ allocated to $t$ by:
* $M$, if $t$ is a constant — that is, $[c]^h_M = I(c) = c_M$
* $h$, if $t$ is a variable — that is, $[x]^h_M = h(x)$
* $M$ and $h$, if $t$ is a term $f(t_1, \dots, t_n)$ — that is, $[t]^h_M = f_M([t_1]^h_M, \dots, [t_n]^h_M)$.

### **Definition 2.7**
1. Let $R$ be an $n$-ary predicate symbol in $L$, and $t_1, \dots, t_n$ be $L$-terms (see Def. 2.1). Let $[t_i]^h_M = a_i$ be the value of $t_i$ in $M$ under $h$ for each $i = 1, \dots, n$.
   $M, h \models R(t_1, \dots, t_n)$ if $(a_1, \dots, a_n) \in R_M$. If not, then $M, h \not\models R(t_1, \dots, t_n)$.
2. Let $t, t'$ be terms. Then
   $M, h \models t = t'$ if $t$ and $t'$ have the same value in $M$ under $h$, that is $[t]^h_M = [t']^h_M$. If they don't, then $M, h \not\models t = t'$.
3. $M, h \models \top$, and $M, h \not\models \bot$.
4. $M, h \models A \land B$ if $M, h \models A$ and $M, h \models B$. Otherwise, $M, h \not\models A \land B$.
   $\dots \lor, A \lor B, A \rightarrow B, A \leftrightarrow B \dots$ as in propositional logic.

We say that $g$ and $h$ are *$x$-equivalent*, written $g =_x h$, if they differ at most in the assignment of $x$.
* The following four variable assignments are $y$-equivalent.
  * $h_1: \quad h_1(x) = a_1, \ h_1(y) = a_2, \ h_1(z) = a_3$
  * $h_2: \quad h_2(x) = a_1, \ h_2(y) = a_4, \ h_2(z) = a_3$
  * $h_3: \quad h_3(x) = a_1, \ h_3(y) = a_5, \ h_3(z) = a_3$
  * $h_4: \quad h_4(x) = a_1, \ h_4(y) = a_1, \ h_4(z) = a_3$

### **Definition 2.8 (Def. 4.8 continued)**
Let $M$ be a $L$-structure and $h$ be any assignment over $M$.
Suppose we already know how to evaluate a formula $\phi$ in $M$ under any assignment. Let $x$ be any variable. Then:
6. $M, h \models \exists x \phi$ if $M, g \models \phi$ for *some* assignment $g$ over $M$ that is $g =_x h$. If not, then $M, h \not\models \exists x \phi$.
7. $M, h \models \forall x \phi$ if $M, g \models \phi$ for *every* assignment $g$ over $M$ that is $g =_x h$. If not, then $M, h \not\models \forall x \phi$.

*Equivalences:*
1. $\forall x \forall y \phi$ is logically equivalent to $\forall y \forall x \phi$.
2. $\exists x \exists y \phi$ is (logically) equivalent to $\exists y \exists x \phi$.
3. $\neg \forall x A$ is equivalent to $\exists x \neg \phi$.
4. $\neg \exists x \phi$ is equivalent to $\forall x \neg \phi$.
5. $\forall x (\phi \land \psi)$ is equivalent to $\forall x \phi \land \forall x \psi$.
6. $\exists x (\phi \lor \psi)$ is equivalent to $\exists x \phi \lor \exists x \psi$.

**$x$ doesn't occur free in $\psi$**
7. $\forall x \phi$ and $\exists x \phi$ are logically equivalent to $\phi$.
   E.g., $\forall x \underbrace{\exists x P(x)}_{\phi}$ and $\exists x \underbrace{\exists x P(x)}_{\phi}$ are equivalent to $\underbrace{\exists x P(x)}_{\phi}$.
8. $\exists x (\phi \land \psi)$ is equivalent to $\phi \land \exists x \psi$, and $\forall x (\phi \lor \psi)$ is equivalent to $\phi \lor \forall x \psi$.
9. $\exists x (\phi \rightarrow \psi)$ is equivalent to $\forall x \phi \rightarrow \psi$, and $\exists x (\psi \rightarrow \phi)$ is equivalent to $\psi \rightarrow \exists x \phi$.
10. Note: if $x$ does not occur free in $\psi$ ($x$ can occur free in $\phi$) then
    $\forall x (\phi \rightarrow \psi)$ is equivalent to $\exists x \phi \rightarrow \psi$, and
    $\exists x (\phi \rightarrow \psi)$ is equivalent to $\forall x \phi \rightarrow \psi$.
    *The quantifier changes! Watch out!*

### **First-Order Natural Deduction Rules**

**$\forall$ I**
1 $|$ $c$ \hspace{2cm} $\forall I$ const
2 $|$ $\vdots$ \hspace{1cm} (the proof) hard struggle
3 $|$ $\phi[c/x]$ \hspace{1cm} we made it!
4 $\forall x \phi \quad \forall I(1,3)$

**$\forall$ E**
1 $\forall x \phi$ \hspace{1cm} got this somehow...
2 $\phi[t/x]$ \hspace{1cm} $\forall E(1)$

**$\exists$ I**
1 $\phi[t/x]$ \hspace{1cm} got this somehow...
2 $\exists x \phi$ \hspace{1cm} $\exists I(1)$

**$\exists$ E**
1 $\exists x \phi$ \hspace{1cm} got this somehow
2 $|$ $c \quad \phi[c/x]$ \hspace{1cm} asm
3 $|$ $\vdots$ \hspace{1cm} (the proof) hard struggle
4 $|$ $\psi$ \hspace{2cm} we made it!
5 $\psi \quad \exists E(1, 2-4)$

**=sub**
1 $\phi[t/x]$ \hspace{1cm} got this somehow...
2 $\vdots$ \hspace{2cm} yada yada yada
3 $t = u$ \hspace{2cm} ...and this
4 $\phi[u/x]$ \hspace{2cm} $=sub(1,3)$

**=sym**
1 $c = d$ \hspace{1cm} premise
2 $d = c \quad =sym(1)$