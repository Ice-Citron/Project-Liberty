------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------
-- SQL DDL: Definition of Tables
------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------

CREATE TABLE branch
(
    sortcode INTEGER NOT NULL,
    bname    VARCHAR(20) NOT NULL,
    cash     DECIMAL(10, 2) NOT NULL 
)

CREATE TABLE account (
    no       INTEGER NOT NULL,
    type     VARCHAR(8) NOT NULL,
    cname    VARCHAR(20) NOT NULL,
    rate     DECIMAL(4, 2) NULL,    
    sortcode INTEGER NOT NULL
)

------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------
-- SQL DDL: Definition of Keys
------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------

CREATE TABLE branch (
    sortcode INTEGER NOT NULL,
    bname VARCHAR(20) NOT NULL,
    cash DECIMAL(10, 2) NOT NULL,
    CONSTRAINT branch_pk PRIMARY KEY (sortcode)
)

CREATE TABLE account (
    no INTEGER NOT NULL,
    type VARCHAR(8) NOT NULL,
    cname VARCHAR(20) NOT NULL,
    rate DECIMAL(4, 2) NOT NULL,
    sortcode INTEGER NOT NULL,
    CONSTRAINT account_pk PRIMARY KEY (no),
    CONSTRAINT account_fk FOREIGN KEY (sortcode) REFERENCES branch
)


-- Declaring Primary Keys after table creation
ALTER TABLE branch
ADD CONSTRAINT branch_pl PRIMARY KEY (sortcode);

-- Declaring Secondary Keys for a table
CREATE UNIQUE INDEX branch_bname_key ON branch(bname);


------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------
-- SQL DML: Inserting, Updating and Deleting Data
------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------

INSERT INTO account
VALUES
(100, 'currrent', 'McBrien, P.', NULL, 67),
(102, 'currrent', 'McBrien, P.', NULL, 67),
(108, 'deposit', 'Sienar, P.', NULL, 67);

UPDATE account
SET    type = 'deposit'
WHERE  num = 100;

DELETE
FROM   account
WHERE  no = 100;


-- SQL DML (Data Manipulation Language): An Implementation of the RA
SELECT branch.bname,
       account.no
FROM   account INNER JOIN branch
ON     account.sortcode = branch.sortcode
WHERE  account.type = 'current';


-- Binary operators between SELECT statements
UNION       -- implements RA /cup
EXCEPT      -- implements RA -
INTERSECT   -- implements RA /cap

SELECT no FROM account
EXCEPT
SELECT no FROM movement;


-- Set or Bag Based Semantics: No Iteration
SELECT hundred.digit * 100 + ten.digit * 10 + unit.digit AS n
FROM   
(VALUES (0), (1), (2), (3), (4), (5), (6), (7), (8), (9)) AS unit(digit)
CROSS JOIN
(VALUES (0), (1), (2), (3), (4), (5), (6), (7), (8), (9)) AS ten(digit)
CROSS JOIN
(VALUES (0), (1), (2), (3), (4), (5), (6), (7), (8), (9)) AS hundred(digit)
ORDER BY n;


-- Common Table Expressions: SQL WITH
WITH decimal(digit) AS (VALUES (0), (1), (2), (3), (4), (5), (6), (7), (8), (9))
SELECT hundred.digit * 100 + ten.digit * 10 + unit.digit AS n
FROM   decimal AS unit
       CROSS JOIN decimal AS ten
       CROSS JOIN decimal AS hundred;


------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------
-- TOPIC 6: SQL BAGS AND SETS
------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------

-- SQL: Bags and Sets
SELECT ALL sortcode
FROM       account;

SELECT DISTINCT sortcode
FROM            account;


------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------
-- Topic 7: Experiment with SQL in DoC
------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------

SELECT bname, sortcode
FROM account a, branch b
WHERE a.sortcode = b.sortcode
  AND type = 'current';

SELECT bname, sortcode
FROM   account a
NATURAL JOIN branch b
WHERE type = 'current';


------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------
-- Topic 8: SQL Set Operations
------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------

-- `IN` operator tests for membership of a set**
SELECT *
FROM   account
WHERE  type = "current"
AND    no IN (100, 101);


-- Can use nested `SELECT` to generate set
SELECT no
FROM account
WHERE type = 'current'
  AND no IN (
    SELECT no
    FROM   movement
    WHERE  amount > 500  
  );

-- Which is equivalent to:
SELECT DISTINCT a.no
FROM   account a
JOIN   movement m
ON a.no = m.no
WHERE type = 'current'
  AND amount > 500 


------ ------ ------ ------ ------ ------ 
-- SET OPERATIONS: EXISTS

/*
-- Testing for Existence
    - `IN` can be used to test if some value is in a relation, either listed,
       or produced by some `SELECT` statement
    - `EXISTS` can be used to test if a `SELECT` statement returns any rows
*/

-- EQUIVALENCE 1 ----
SELECT DISTINCT cname
FROM   account
WHERE  cname NOT IN (
    SELECT cname
    FROM   account
    WHERE  type='deposit'
)

-- EQUIVALENCE 2 ----
SELECT DISTINCT cname
FROM   account
WHERE NOT EXISTS (
    SELECT 1
    FROM   account AS deposit_account
    WHERE  type='deposit'
    AND    account.cname = deposit_account.cname
)


------ ------ ------ ------ ------ ------ 
-- SQL CORRELATED SUBQUERY

/*
    A subquery that references the outer query columns is called a correlated
    subquery

    Semantics is as if the subquery is executed for each row of the outer query
    WHERE clause
*/

SELECT DISTINCT cname
FROM   account
WHERE  NOT EXISTS (
    SELECT 1
    FROM  account AS deposit_account
    WHERE type = 'deposit'
      AND account.cname = deposit_account.cname
)

-- Outer Query: Finds All Names
SELECT DISTINCT cname
FROM   account

-- Subquery Execution for 'Boyd. M.'
SELECT 1
FROM   account AS deposit_account
WHERE  type = 'deposit'
  AND  'Boyd, M.' = deposit_account.cname


------ ------ ------ ------ ------ ------ 
-- SET OPERATIONS: `EXISTS`

/*
    NOT EXISTS and EXCEPT
        - Most queries involving EXCEPT can be also written using NOT EXISTS
        - EXCEPT relatively recent addition to SQL
*/

-- Equivalence 1 ----
SELECT no FROM account
EXCEPT
SELECT no FROM movement

-- Equivalence 2 ----
SELECT no FROM account
WHERE NOT EXISTS (
    SELECT 1
    FROM movement
    WHERE movement.no = account.no
)


------ ------ ------ ------ ------ ------ 
-- SET OPERATIONS: SOME and ALL

/*
    Can test a value against members of a set
        - V op SOME S is TRUE is there is at least one V_s ∈ S s.t. V op V_s
        - V op ALL S is TRUE is there are no values V_s ∈ S s.t. NOT V op V_s
*/

-- names of branches that only have current ccounts
SELECT bname
FROM   branch
WHERE  'current' = ALL (
    SELECT type
    FROM   account
    WHERE  branch.sortcode = account.sortcode
)

-- names of branches that have deposit accounts
SELECT bname
FROM   branch
WHERE  'deposit' = SOME (
    SELECT type
    FROM   account
    WHERE  branch.sortcode = account.sortcode
)


------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
/*
    Write an SQL query without using any negation (i.e. without the use of NOT
    or EXCEPT) that list accounts with no movements on or before the 11-Jan-1999
*/
SELECT no
FROM   account
WHERE '11-Jan-1999' < ALL (
    SELECT tdate
    FROM   movement
    WHERE  movement.no = account.no
)


/*
    Write an SQL query that lists the `cname` of customers that have every type
    of account that appears in account
*/
SELECT DISTINCT cname
FROM account a
WHERE NOT EXISTS (
    SELECT DISTINCT type FROM account b
    EXCEPT
    SELECT type FROM account c
    WHERE c.cname = a.cname
) 

SELECT DISTINCT cname
FROM account a
WHERE NOT EXISTS (
    SELECT type
    FROM   account b
    WHERE  NOT EXISTS (
        SELECT 1
        FROM account c
        WHERE a.cname = c.cname
          AND b.type = c.type
    )
)

/*
    1. works through double negation. If type1 exists in 3rd-nested, returns 
       nothing for 2nd-nested. Hence first-nested is True.
    2. If type1 does not exist in 3rd-nested, then, returns True for 2nd-nested,
       this is False for 1st-nested. Hence object isn't returned at first-nested
    3. At the third-nested, list relies on WHERE and references to previous
       nests... by filtering down or constraining ith-cname vs kth-cname, 
       and jth-type vs kth-type.

    - ith for 1st-nest // jth for 2nd-nest // kth for 3rd-nest

    ---

    - 2nd-nest seems to contain the full rows of divisor
*/



------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
-- EXERCISE - DIVISOR
------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 

                        ------ ------ ------
-- QUESTION 1
SELECT DISTINCT sname
FROM student
WHERE NOT EXISTS (
    SELECT course.cid
    FROM course
    WHERE course.department = "Computing"
      AND NOT EXISTS (
        SELECT 1 
        FROM enrollment
        WHERE student.sid = enrollment.sid
          AND course.cid  = enrollment.cid
    )
)

SELECT DISTINCT sname
FROM student m
WHERE NOT EXISTS (
    SELECT d.cid FROM course d WHERE d.department = 'Computing'
    EXCEPT
    SELECT r.cid FROM enrollment r WHERE m.sid = r.sid
)


                        ------ ------ ------
-- QUESTION 2
SELECT DISTINCT ename
FROM employee m
WHERE NOT EXISTS (
    SELECT skill_id
    FROM employee_skills d
    WHERE eid = (SELECT eid FROM m WHERE ename="Frank Miller")
      AND NOT EXISTS (
        SELECT 1
        FROM employee_skills r
        WHERE d.skill_id = r.skill_id
          AND m.eid = r.eid
      )
)

SELECT DISTINCT ename
FROM employee m
WHERE NOT EXISTS (
    SELECT skill_id FROM employee_skills d 
      WHERE d.eid = (SELECT eid FROM employee WHERE ename="Frank Miller")
    EXCEPT
    SELECT skill_id FROM employee_skills r WHERE r.eid = m.eid
)


                        ------ ------ ------
-- QUESTION 3
SELECT DISTINCT ename
FROM employee e
WHERE NOT EXISTS (
    -- Skills that 3+ employees have
    SELECT   skill_id
    FROM     employee_skill
    GROUP BY skill_id
    HAVING   COUNT(*) >= 3

    EXCEPT
    
    -- skills that each employee e has
    SELECT skill_id
    FROM   employee_skill es
    WHERE  es.eid = e.eid
)

SELECT DISTINCT ename
FROM employee m
WHERE NOT EXISTS
    SELECT skill_id
    FROM (
        SELECT   skill_id
        FROM     employee_skill
        GROUP BY skill_id
        HAVING   COUNT(*) >= 3
    ) d
    WHERE NOT EXISTS (
        SELECT 1
        FROM employee_skill r
        WHERE d.skill_id = r.skill_id
          AND m.eid      = r.eid
    )

                        ------ ------ ------

------ ------ ------ ------ ------ ------ 
-- TEMPLATE -----

-- EXCEPT method (cleaner)
SELECT DISTINCT [target]
FROM   [main_table] m
WHERE NOT EXISTS (
    SELECT [attr] FROM [divisor_set]
    EXCEPT
    SELECT [attr] FROM [relatioship] r WHERE r.id = m.id
)

-- Double NOT EXISTS method (more explicit)
SELECT DISTINCT [target]
FROM   [main_table] m
WHERE NOT EXISTS (
    SELECT [attr] FROM [divisor_set] d
    WHERE NOT EXISTS (
        SELECT 1 FROM [relationship] r
        WHERE r.id = m.id AND d.attr = r.attr
    )
)

------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 

-- Equivalence 1 -----
SELECT no
FROM   account
WHERE  500 >= ALL (
    SELECT amount
    FROM   movement
    WHERE  account.no = movement.no
)

-- Equivalence 2 -----
SELECT no
FROM   account
WHERE  NOT 500 < SOME (
    SELECT amount
    FROM   movement
    WHERE  account.no = movement.no
)

------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 

-- R(A) and S(B), A and B are not nullable

-- Equivalence 1 -----
SELECT A
FROM   R
EXCEPT
SELECT B
FROM   S

-- Equivalence 2 -----
SELECT A
FROM   R
WHERE NOT EXISTS (
    SELECT *
    FROM   S
    WHERE  S.B=R.A
)

-- Equivalence 3 -----
SELECT A
FROM   R
WHERE  A NOT IN (SELECT B FROM S)

------ ------ ------ ------ ------ ------

-- R(A) and S(B), A or B are nullable

-- NOT Equivalence 1 -----
SELECT A FROM R
EXCEPT
SELECT B FROM S

-- NOT Equivalence 2 -----
SELECT A
FROM   R
WHERE NOT EXISTS (
    SELECT *
    FROM   S
    WHERE  S.B=R.A
)

-- NOT Equivalence 3 -----
SELECT A
FROM   R
WHERE  A NOT IN (SELECT B FROM S)


