Hello world 

> module DataTypes where


SIMPLE / ENUMERATION TYPE
> data Direction = North | South | East

PRODUCT TYPE (multiple fields)
> data Point = MkPoint Float Float

SUM & PRODUCT TYPES
> data Tree a = Leaf a | Node (Tree a) (Tree a)




The `newtype` keyword is used to create a distinct type for an existing type,
restricted to having a SINGLE CONSTRUCTOR WITH A SINGLE FIELD. 
- ZERO RUNTIME OVERLOAD: Unlike `data`, the wrapper is completely erased at
  compile time. 
- DIFFERENT TYPECLASS INSTAMNCES: Used to give an existing type a different
  behavior (e.g. defining a new `Monoid` instance for `Int`)
> 
