-- Here's a quick example of using `ESCAPE` in SQL pattern matching

-- Scenario: Find accouints with names that contain an actual underscore 
-- character (not using _ as a wildcard)

SELECT *
FROM account
WHERE cname LIKE '%\_%' ESCAPE '\';

/*
    How it works:
        - Normally, `_` is a wildcard that matches any single character
        - By using `ESCAPE '\'`, we tell sql that `\` is our escape character
        - So `\_` means "match a literal underscore" not "match any character"
*/


-- Another example with %:

-- Find branch names that literally contain 100% in the text
SELECT bname
FROM branch
WHERE bname LIKE '%100\%%' ESCAPE '\';

/*
    This would match a branch named "Guaranteed 100% Returns" because:
        - First `%` = match anything before
        - `100` = match literal "100"
        - `\%` = match literal % symbol (not a wildcard)
        - Last `%` = match anything after
    Without `ESCAPE`, you couldn't search for literal `%` or `_` characters in
    your data!
*/

-- You can use any character as your escape character too. Not just `\`!


------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 

SELECT name AS country,
       capital,
       population
FROM country
WHERE population >= 100000000
ORDER BY population DECS;   -- sort by population in descending order

-- Note: The capital field is nullable (indicated by `?` in the schema), so
--       some çountries might have NULL for their capital.





