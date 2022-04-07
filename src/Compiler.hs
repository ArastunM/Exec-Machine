module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Data.Map
import Machine
import Interpreter


--TODO Task 3.1
-- acomp compiles an arithmetic expression to a sequence of machine instructions
acomp :: AExp -> [Instr]
acomp (N x) = [LOADI x]
acomp (V v) = [LOAD v]
acomp (Plus a1 a2) = acomp a1 ++ acomp a2 ++ [ADD]

--TODO Task 3.2
-- bcomp compiles a boolean expression to a sequence of machine instructions
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc const) b x | const == b = [JMP x]
                     | otherwise = []
bcomp (Not b1) b x = bcomp b1 (not b) x
-- if b is true then we use JMPLESS, otherwise we use JMPGE (the reason is to align the compiler properly with exec)
bcomp (Less a1 a2) b x | b = acomp a1 ++ acomp a2 ++ [JMPLESS x]
                       | otherwise = acomp a1 ++ acomp a2 ++ [JMPGE x]

-- NOTE: long implementation (lines 30 - 78)
bcomp (And b1 b2) b x | b = bcompANDTrue b1 b2 x | otherwise = bcompANDFalse b1 b2 x


-- AUXILIARY FUNCTIONS of bcomp (And b1 b2)
-- used when bcomp's second parameter is True
bcompANDTrue :: BExp -> BExp -> Int -> [Instr]
bcompANDTrue b1 b2 x | (jmpOccurs (bcomp b1 True x) x) &&
                         (jmpOccurs (bcomp b2 True x) x) = (bcompANDWithLess b1 b2 True x)
                     | otherwise = []
-- used when bcomp's second parameter is False
bcompANDFalse :: BExp -> BExp -> Int -> [Instr]
bcompANDFalse b1 b2 x | ((not (containsLess b1)) && (jmpOccurs (bcomp b1 False x) x)) ||
                          ((not (containsLess b2)) && (jmpOccurs (bcomp b2 False x) x)) = [JMP x]
                      | otherwise =
                         if ((not (containsLess b1)) && (jmpOccurs (bcomp b1 False x) x)) ||
                            ((not (containsLess b2)) && (jmpOccurs (bcomp b2 False x) x)) then [JMP x]
                         else (bcompANDWithLess b1 b2 False x)

-- used when the boolean expression "LESS" should be considered
bcompANDWithLess :: BExp -> BExp -> Bool -> Int -> [Instr]
bcompANDWithLess b1 b2 b x
  | (containsLess b1) && (containsLess b2) =
    if b then (init (bcomp b1 b x)) ++ [JMPLESS 1, JMP (length (bcomp b2 b x))] ++ (bcomp b2 b x)
    else (init (bcomp b1 b x)) ++ [JMPGE ((length (bcomp b2 b x)) + 1 + x), JMP (length (bcomp b2 b x))] ++ (bcomp b2 b x)
  | (containsLess b1) = (bcomp b1 b x)
  | (containsLess b2) = (bcomp b2 b x)
  | otherwise = if b then [JMP x] else []

-- simulates the program counter of iexec
getCounter :: Instr -> Int -> Int
getCounter (JMP i) c = c + 1 + i
getCounter (JMPLESS i) c = c + 1 + i
getCounter (JMPGE i) c = c + 1 + i
getCounter _ c = c + 1

-- simulates the program counter of exec
getFinalCounter :: [Instr] -> Int -> Int
getFinalCounter fs c | (getInstr fs (c,empty,[])) == [] = c
                     | otherwise = getFinalCounter fs
                       (getCounter (head (getInstr fs (c,empty,[]))) c)

-- determines if given amount of jmp (final jmp) would occur in given instructions
jmpOccurs :: [Instr] -> Int -> Bool
jmpOccurs fs x = (length fs) + x == (getFinalCounter fs 0)

-- determines if given boolean expression contains "LESS"
containsLess :: BExp -> Bool
containsLess (Less a1 a2) = True
containsLess (And b1 b2) = (containsLess b1) || (containsLess b2)
containsLess _ = False

--TODO Task 3.3
-- ccomp compiles a high-level program to a sequence of machine instructions
ccomp :: Com -> [Instr]
ccomp (Assign v x) = acomp x ++ [STORE v]
ccomp (Seq c1 c2) = ccomp c1 ++ ccomp c2

-- if b is false, jump over c1 (and the unconditional jump after c1) and execute c2 only
ccomp (If b c1 c2) = (bcomp b False (length (ccomp c1) + 1)) ++
                      ccomp c1 ++ [JMP (length (ccomp c2))] ++ ccomp c2
-- when b is false, jump over c (and the loop jump) terminating the run
-- while b is true, execute c and jump back to the beginning (over the last jump itself, c and b)
ccomp (While b c) = (bcomp b False (length (ccomp c) + 1)) ++
                     ccomp c ++ [JMP (0 - length (ccomp c)
                                        - length (bcomp b False 0) - 1)]
ccomp (SKIP) = []
