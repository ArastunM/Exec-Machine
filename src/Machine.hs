module Machine
(      
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec,
        getInstr
) where

import Data.Map


--TODO Task 1.1
-- Vname models variable names (as Strings)
type Vname = [Char]
--TODO Task 1.2
-- Val models variable values (as Integers)
type Val = Int
--TODO Task 1.3
-- State maps variable names (Vname) to values (Val)
type State = Map Vname Val

--TODO Task 1.4
-- Instr models all instructions supported by the machine
-- LOADI x, LOAD v, ADD, STORE v, JMP i, JMPLESS i, JMPGE i
data Instr = LOADI Val | LOAD Vname | ADD | STORE Vname |
             JMP Int | JMPLESS Int | JMPGE Int
             deriving (Eq, Read, Show)

--TODO Task 1.5
-- Stack keeps a temporary list of values
type Stack = [Val]

--TODO Task 1.6
-- Config models the low-level machine's configuration
-- Contains: Program Counter value, State, Stack
type Config = (Int, State, Stack)

--TODO Task 1.7
-- iexec executes a single instruction
iexec :: Instr -> Config -> Config
-- LOADI x loads a value x onto the stack, increments the counter by one
iexec (LOADI x) (c, state, stack) = (c + 1, state, [x] ++ stack)
-- LOAD v loads the value of a variable v onto the stack, increments the counter by one
iexec (LOAD v) (c, state, stack) = (c + 1, state, [state ! v] ++ stack)

-- ADD adds the two topmost values of the stack, increments the counter by one
iexec (ADD) (c, state, stack) =
            (c + 1, state,
            [sum (Prelude.take 2 stack)] ++ Prelude.drop 2 stack)
-- STORE v stores the top of the stack to variable v, increments the counter by one
iexec (STORE v) (c, state, stack) =
                (c + 1, insert v (head stack) state, Prelude.drop 1 stack)

-- JMP i increments the counter by one and then by i
iexec (JMP i) (c, state, stack) = (c + 1 + i, state, stack)
-- JMPLESS i increments the counter by one
-- compares the two topmost values x and y, in case of x > y increments the counter by i
iexec (JMPLESS i) (c, state, stack) =
                   if head stack > head (tail stack) then (c + 1 + i, state, Prelude.drop 2 stack)
                   else (c + 1, state, Prelude.drop 2 stack)
-- JMPGE i same as JMPLESS except x <= y is compared
iexec (JMPGE i) (c, state, stack) =
                 if head stack <= head (tail stack) then (c + 1 + i, state, Prelude.drop 2 stack)
                 else (c + 1, state, Prelude.drop 2 stack)

--TODO Task 1.8
-- exec executes a list of instructions
-- instructions are executed based on current counter value
exec :: [Instr] -> Config -> Config
exec fs conf | (getInstr fs conf) == [] = conf
             | otherwise = exec fs
               (iexec (head (getInstr fs conf)) conf)

-- returns list of instructions to be executed, based on program counter
getInstr :: [Instr] -> Config -> [Instr]
getInstr [] _ = []
getInstr fs (0,_,_) = fs
getInstr (f:fs) (c, state, stack) = getInstr fs (c - 1, state, stack)
