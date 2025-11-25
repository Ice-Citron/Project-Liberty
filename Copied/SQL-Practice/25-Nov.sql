INSERT INTO account
(126, 'buisness', 'McBrien, P.', 1.00, 67)

------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------

CROSS JOIN 
INNER JOIN ON theta
NATURAL JOIN
USING(k)

LEFT OUTER JOIN
RIGHT OUTER JOIN
FULL OUTER JOIN

LEFT SEMI JOIN
RIGHT SEMI JOIN
LEFT ANTI JOIN
RIGHT ANTI JOIN

SELF JOIN

------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------

/*
In SQL, the rename operator is simply the keyword `AS`.
*/

SELECT *
FROM R AS r1        -- or R r1
JOIN R AS r2        -- or R r2
ON ...              -- This is the theta condition

------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------


/*
HOW DIVISION WORK <-- MY INTUITION

ah i can kinda see how division works. now... it works similar to WHERE NOT 
EXISTS i presume?

and it primarily works through listing out all possible combinations first. and 
then subtracting the stuff that exists inside R... which what's leftover is... i 
mean, if theres stuff left over from RXS - R... then this means that these rows 
are incomplete...

and then u project, and then subtract these incomplete rows from the list of all
possible rows... and what's left are the rows thats complete?
*/


------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------

-- 1. PROJECT AND PROJECT
    -- π_X π_Y R ≡ π_X R    (if X⊆Y)

-- Inefficient (nested projection)
SELECT name, age
FROM (
    SELECT name, age, salary, dept
    FROM employee
) AS temp;

-- Equivalent and better
SELECT name, age
FROM employee;

-- **Use:** Simplifies nested queries - eliminate redundant intermediate 
--          projections.

------ ------ ------ ------ ------ ------

-- 2. PROJECT AND SELECT
    -- π_X σ_P(Y) R ≡ σ_P(Y) π_X R    (if Y⊆X)
    
    -- Meaning: You can swap SELECT and PROJECT if the condition uses only
    --          projected attributes.

-- Original (filter then project)
SELECT name, salary
FROM (
    SELECT *
    FROM employee
    WHERE salary > 50000
) AS temp;

-- Equivalent (project then filter)
SELECT name, salary
FROM employee
WHERE salary > 50000

-- **Use:** Push selection down or up depending on what's more efficient.

------ ------ ------ ------ ------ ------

-- 3. PROJECT AND PRODUCT (Cartesian Product)
    -- π_X (R × S) ≡ π_(X∩Attrs(R)) R × π_(X∩Attrs(S)) S

    -- Meaning: When projecting from a product, you can project each table 
    --          separately first.

-- Inefficient (full cross product then project)
SELECT account.num, branch.bname
FROM account
CROSS JOIN branch;

-- Equivalent (project first, then cross product) - way better!
SELECT a.num, b.bname
FROM (SELECT num FROM account) AS a
CROSS JOIN (SELECT bname FROM branch) AS b;

-- Use: Reduce data sie before expensive operations. Project early to minimize
--      intermediate result size.

------ ------ ------ ------ ------ ------

-- 4. PROJECT AND UNION
    -- π_X (R ∪ S) ≡ π_X R ∪ π_X S

    -- Meaning: Projecting a union = union of projections.

-- Version 1 (union then project)
SELECT ename, salary
FROM (
    SELECT * FROM employee_2024
    UNION
    SELECT * FROM employee_2025
);

-- Equivalent Version 2 (project then union)
SELECT ename, salary FROM employee_2024
UNION                             -- union does not allow for duplicate elements
SELECT ename, salary FROM employee_2025;

-- Use: Version 2 is usually better - projects before combining, reducing data
--      movment

------ ------ ------ ------ ------ ------

-- 5. PROJECT AND DIFFERENCE
    -- π_X (R - S) ⊇ π_X R - π_X S

    -- Meaning: NOT EQUAL! (note the ⊇ symbol). LHS has more or equal rows than
    --          RHS.

-- Use: Be carful! You **cannot** always push projections through differences.
--      LHS is safer.

------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
-- EQUIVALENCES INVOLVING SELECT (\sigma)
------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 

-- 1. SELECT AND PROJECT

-- 2. SELECT AND SELECT

SELECT *
FROM (
    SELECT *
    FROM employee
    WHERE salary > 50000
)
WHERE dept = "Sales";

SELECT *
FROM employee
WHERE salary > 50000 AND depth = "Sales";

-- Use: Combine multiple WHERE conditions into one. Database can optimise 
--      single condition better.

------ ------ ------ ------ ------ ------

-- 3. SELECT AND PRODUCT
    -- σ_P(X) (R × S) ≡ σ_P(X) R × S    (if X⊆Attrs(R))

    -- Meaning: If filter only uses attributes from R, filter R first before 
    --          the product.

-- Inefficient (cross product then filter)
SELECT *
FROM account
CROSS JOIN branch
WHERE account.sortcode = 56;

-- Equivalent and much better! (filter then cross product)
SELECT *
FROM (SELECT * FROM account WHERE account.sortcode = 56) a
CROSS JOIN branch b;

-- Use: Critical optimization! Filter before joining to reduce intermediate
--      size dramatically.

------ ------ ------ ------ ------ ------

-- 4. SELECT AND UNION
    -- σ_P(X) (R ∪ S) ≡ σ_P(X) R ∪ σ_P(X) S

    -- Meaning: Filtering a union = union of filtered results

-- Version 1 (union then filter)
SELECT *
FROM (
    SELECT * FROM employee_2024
    UNION
    SELECT * FROM employee_2025
)
WHERE dept = "sales"

-- Equivalent Version 2 (filter then union) - often better! 
SELECT * FROM employee_2024 WHERE dept = "sales"
UNION 
SELECT * FROM employee_2025 WHERE dept = "sales"

-- Use: Push filters down to each branch - can use indexes on individual tables!

------ ------ ------ ------ ------ ------

-- 5. SELECT AND DIFFERENCE
    -- σ_P(X) (R - S) ≡ σ_P(X) R - S

    -- Meaning: Filtering a difference = filter first relation, then subtract

SELECT *
FROM (
    SELECT * FROM all_employees
    EXCEPT
    SELECT * FROM terminated_employees
)
WHERE dept = 'sales';

SELECT *
FROM (SELECT * FROM all_employees WHERE dept = 'sales')
EXCEPT (SELECT * FROM terminated_employees);

-- Use: Filter the first relation early to reduce data before set difference
--      (which is an expensive operation, I presume).

------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------
-- Applying in Practice
------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------

-- Bad Query
SELECT e.name, e.salary
FROM employee e
CROSS JOIN department d
WHERE e.dept_id = d.id
  AND d.name = 'sales'
  AND e.salary > 50000

-- Apply equivalences:
-- 1. Push selection down (Select and Product)
-- 2. Turn cross product into proper join
-- 3. Project early (Project)
SELECT e.name, e.salary
FROM (
    SELECT name, salary, dept_id
    FROM employee
    WHERE salary > 50000
) e
JOIN (
    SELECT id
    FROM department
    WHERE name = 'sales'
) d
ON e.dept_id = d.id