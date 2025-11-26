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
(108, 'deposit', 'Sienar, P.', NULL, 67)

UPDATE account
SET    type = 'deposit'
WHERE  num = 100

DELETE
FROM   account
WHERE  no = 100


-- SQL DML (Data Manipulation Language): An Implementation of the RA
SELECT branch.bname,
       account.no
FROM   account INNER JOIN branch
ON     account.sortcode = branch.sortcode
WHERE  account.type = 'current'


-- Binary operators between SELECT statements
UNION       -- implements RA /cup
EXCEPT      -- implements RA -
INTERSECT   -- implements RA /cap

SELECT no FROM account
EXCEPT
SELECT no FROM movement


-- Set or Bag Based Semantics: No Iteration
SELECT hundred.digit * 100 + ten.digit * 10 + unit.digit AS n
FROM   
(VALUES (0), (1), (2), (3), (4), (5), (6), (7), (8), (9)) AS unit(digit)
CROSS JOIN
(VALUES (0), (1), (2), (3), (4), (5), (6), (7), (8), (9)) AS ten(digit)
CROSS JOIN
(VALUES (0), (1), (2), (3), (4), (5), (6), (7), (8), (9)) AS hundred(digit)
ORDER BY n


-- Common Table Expressions: SQL WITH
WITH decimal(digit) AS (VALUES (0), (1), (2), (3), (4), (5), (6), (7), (8), (9))
SELECT hundred.digit * 100 + ten.digit * 10 + unit.digit AS n
FROM   decimal AS unit
       CROSS JOIN decimal AS ten
       CROSS JOIN decimal AS hundred


------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------
-- TOPIC 6: SQL BAGS AND SETS
------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------

-- SQL: Bags and Sets
SELECT ALL sortcode
FROM       account

SELECT DISTINCT sortcode
FROM            account


------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------
-- Topic 7: Experiment with SQL in DoC
------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------