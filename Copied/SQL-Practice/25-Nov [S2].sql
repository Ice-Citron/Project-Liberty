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
    CONSTRAINT account_pk PRIMARY KEY no,
    CONSTRAINT account_fk FOREIGN KEY sortcode REFERENCES branch
)