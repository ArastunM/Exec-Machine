module Interpreter
(
    AExp(..),
    BExp(..),
    Com (..),
    aval,
    bval,
    eval
) where

import Data.Map
import Machine


--TODO Task 2.1
-- Arithmetic expression
-- N x, V v, Plus a1 a2
data AExp = N Val | V Vname | Plus AExp AExp
            deriving (Eq, Read, Show)

--TODO Task 2.2
-- aval evaluates an arithmetic expression based on a given state
aval :: AExp -> State -> Val
-- N x returns the value x
aval (N x) state = x
-- V v returns the value of the variable v
aval (V v) state = state ! v
-- Plus a1 a2 returns the sum of two arithmetic expressions
aval (Plus a1 a2) state = aval a1 state + aval a2 state

--TODO Task 2.1
-- Boolean expression
-- Bc b (True and False), Not b, And b1 b2, Less a1 a2
data BExp = Bc Bool | Not BExp | Less AExp AExp | And BExp BExp
            deriving (Eq, Read, Show)

--TODO Task 2.3
-- bval evaluates a boolean expression based on a given state
bval :: BExp -> State -> Bool
-- Not b negates the boolean expression b
bval (Not b) state = not (bval b state)
-- Less a1 a2 returns the condition of a1 < a2
bval (Less a1 a2) state = (aval a1 state) < (aval a2 state)
-- And b1 b2 returns the conjunction of two boolean expressions
bval (And b1 b2) state = (bval b1 state) && (bval b2 state)


--TODO Task 2.1
-- Commands
-- Assign v x, Seq c1 c2, If b c1 c2, While b c, SKIP
data Com = Assign Vname AExp | Seq Com Com | If BExp Com Com |
           While BExp Com | SKIP
           deriving (Eq, Read, Show)

--TODO Task 2.4
-- eval evaluates commands
eval :: Com -> State -> State
-- Assign v x assigns the outcome of the arithmetic expression x to variable v
eval (Assign v x) state = insert v (aval x state) state
-- Seq c1 c2 executes the commands c1 and c2 sequentially
eval (Seq c1 c2) state = eval c2 (eval c1 state)

-- If b c1 c2 evaluates the command c1 if boolean expression b is True, otherwise it evaluates c2
eval (If b c1 c2) state | bval b state = eval c1 state
                        | otherwise = eval c2 state
-- While b c evaluates the command c as long as the boolean expression b is True
eval (While b c) state | bval b state = eval (While b c) (eval c state)
                       | otherwise = state
-- SKIP an empty command that does nothing
eval (SKIP) state = state
