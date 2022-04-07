import Test.Tasty
import Test.Tasty.HUnit
import Machine
import Interpreter
import Compiler
import Data.Map
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [machine,machineAdvanced,
                           interpreter,interpreterAdvanced,
                           compiler,compilerAdvanced]

machine = testGroup "Machine Tests"
  [ testCase "Single Instruction" $
    (1,fromList [],[5]) @=? iexec (LOADI 5) (0, empty, [])
  , testCase "Single Instruction" $
    (1,fromList [("v1",5)],[5]) @=? iexec (LOAD "v1") (0, fromList [("v1",5)], [])
  , testCase "Single Instruction" $
    (1,fromList [],[11]) @=? iexec ADD (0, empty, [5,6])
  , testCase "Single Instruction" $
    (1,fromList [("x",5)],[]) @=? iexec (STORE "x") (0, empty, [5])
  , testCase "Single Instruction" $
    (6,fromList [],[]) @=? iexec (JMP 5) (0, empty, [])
  , testCase "Single Instruction" $
    (1,fromList [],[]) @=? iexec (JMPLESS 5) (0, empty, [5,6])
  , testCase "Single Instruction" $
    (6,fromList [],[]) @=? iexec (JMPGE 5) (0, empty, [5,6])
  , testCase "Multiple Instructions" $
    (3,fromList [],[3]) @=? exec [LOADI 1, LOADI 2, ADD] (0,empty,[])
  , testCase "Multiple Instructions" $
    (4,fromList [("v1",1),("v2",2)],[]) @=? exec [LOADI 1, STORE "v1", LOADI 2, STORE "v2"] (0,empty,[])
  ]

interpreter = testGroup "Interpreter Tests"
  [ testCase "Arithmetic Expressions" $
    3 @=? aval (Plus (N 3) (V "x")) (fromList [("x",0)])
  , testCase "Boolean Expressions" $
    False @=? bval (Less (N 3) (V "x")) (fromList [("x",0)])
  , testCase "Commands" $
    fromList [] @=? eval SKIP (fromList [])
  , testCase "Commands" $
    fromList [("x",5)] @=? eval (Assign "x" (N 5)) (fromList [("x",0)])
  , testCase "Commands" $
    fromList [("x",6)] @=? eval (Seq (Assign "x" (N 5)) (Assign "x" (N 6))) (fromList [("x",0)])
  , testCase "Commands" $
    fromList [("x",6)] @=? eval (If (Less (V "x") (N 5)) (Assign "x" (N 6)) (SKIP)) (fromList [("x",4)])
  , testCase "Commands" $
    fromList [("x",10)] @=? eval (If (Less (V "x") (N 5)) (Assign "x" (N 6)) (SKIP)) (fromList [("x",10)])    
  , testCase "Commands" $
    fromList [("x",5)] @=? eval (While (Less (V "x") (N 5)) (Assign "x" (Plus (V "x") (N 1)))) (fromList [("x",0)])
  ]

compiler = testGroup "Compiler Tests"
  [ testCase "Arithmetic Expressions" $
    [LOADI 5,LOAD "x",ADD] @=? acomp (Plus (N 5) (V "x"))
  , testCase "Boolean Expressions" $
    [JMP 3] @=? bcomp (Bc True) True 3
  , testCase "Boolean Expressions" $
    [JMP 3] @=? bcomp (Bc False) False 3
  , testCase "Boolean Expressions" $
    [] @=? bcomp (Bc True) False 3
  , testCase "Boolean Expressions" $
    [] @=? bcomp (Not (Bc False)) False 3
  , testCase "Boolean Expressions" $
    [] @=? bcomp (And (Bc True) (Bc False)) True 3
  , testCase "Boolean Expressions" $
    [] @=? bcomp (And (Bc False) (Bc True)) True 3
  , testCase "Boolean Expressions" $
    [JMP 3] @=? bcomp (And (Bc True) (Bc False)) False 3
  , testCase "Boolean Expressions" $
    [JMP 3] @=? bcomp (And (Bc False) (Bc True)) False 3
  , testCase "Boolean Expressions" $
    [LOAD "x",LOADI 5,JMPLESS 3] @=? bcomp (Less (V "x") (N 5)) True 3
  , testCase "Boolean Expressions" $
    [LOAD "x",LOADI 5,JMPGE 3] @=? bcomp (And (Less (V "x") (N 5)) (Bc True)) False 3
  , testCase "Boolean Expressions" $
    [] @=? bcomp (And (Bc False) (Less (V "x") (N 5))) True 3
  , testCase "Boolean Expressions" $
    [JMP 3] @=? bcomp (And (Bc False) (Less (V "x") (N 5))) False 3
  , testCase "Commands" $
    [LOAD "u",LOADI 1,JMPGE 5,LOAD "u",LOADI 1,ADD,STORE "u",JMP 2,LOAD "u",STORE "v"] @=? ccomp (If (Less (V "u") (N 1)) (Assign "u" (Plus (V "u") (N 1))) (Assign "v" (V "u")))
  , testCase "Commands" $
    [LOAD "u",LOADI 1,JMPGE 5,LOAD "u",LOADI 1,ADD,STORE "u",JMP (-8)] @=? ccomp (While (Less (V "u") (N 1)) (Assign "u" (Plus (V "u") (N 1))))
  ]


-- to be deleted before submission
machineAdvanced = testGroup "Advanced Machine Tests"
  [ testCase "Single Instruction" $
    (1,fromList [],[5,10]) @=? iexec (LOADI 5) (0, empty, [10])
  , testCase "Single Instruction" $
    (1,fromList [("v1",3)],[3,15,10]) @=? iexec (LOAD "v1") (0, fromList [("v1",3)], [15,10])
  , testCase "Single Instruction" $
    (1,fromList [],[11,3,1]) @=? iexec ADD (0, empty, [5,6,3,1])
  , testCase "Single Instruction" $
    (1,fromList [("x",5)],[3,8]) @=? iexec (STORE "x") (0, empty, [5,3,8])
  , testCase "Single Instruction" $
    (9,fromList [],[2,5]) @=? iexec (JMP 5) (3, empty, [2,5])
  , testCase "Single Instruction" $
    (1,fromList [],[10]) @=? iexec (JMPLESS 5) (0, empty, [5,6,10])
  , testCase "Single Instruction" $
    (6,fromList [],[1,6]) @=? iexec (JMPGE 5) (0, empty, [5,6,1,6])
  , testCase "Single Instruction" $
    (6,fromList [],[]) @=? iexec (JMPGE 5) (0, empty, [6,6])
  , testCase "Multiple Instructions" $
    (4,fromList[("a1",10)],[3,10]) @=? exec [LOAD "a1", LOADI 1, LOADI 2, ADD] (0,fromList[("a1",10)],[])
  , testCase "Multiple Instructions JMP" $
    (5,fromList[("a1",10)],[11]) @=? exec [LOAD "a1", LOADI 1, ADD, JMP 1, LOADI 2] (0,fromList[("a1",10)],[])
  , testCase "Multiple Instructions JMPLESS" $
    (8,empty,[]) @=? exec [LOADI 2, LOADI 7, JMPLESS 5, LOADI 1, STORE "x"] (0,empty,[])
  , testCase "Multiple Instructions JMPGE" $
    (5,fromList[("x",1)],[]) @=? exec [LOADI 2, LOADI 7, JMPGE 5, LOADI 1, STORE "x"] (0,empty,[])
  ]

interpreterAdvanced = testGroup "Advanced Interpreter Tests"
  [ testCase "Arithmetic Expressions" $
    8 @=? aval (Plus (Plus (V "y") (N 2)) (V "x")) (fromList [("x",1), ("y",5)])
  , testCase "Boolean Expressions" $
    False @=? bval (And (Not (Less (N 5) (N 6))) (Less (N 3) (V "x"))) (fromList [("x",0)])
  , testCase "Commands" $
    fromList [("x",1), ("y",1)] @=? eval (While (And (Less (V "x") (N 1)) (Less (V "y") (N 5)))
                                 (Seq (Assign "x" (Plus (V "x") (N 1))) (Assign "y" (Plus (V "y") (N 1)))))
                                 (fromList [("x",0), ("y", 0)])
  ]

compilerAdvanced = testGroup "Advanced Compiler Tests"
  [ testCase "Arithmetic Expressions" $
    (5,fromList[("x", 18)],[23]) @=? exec (acomp (Plus (Plus (N 3) (N 2)) (V "x"))) (0,fromList[("x", 18)],[])
  , testCase "Boolean Expressions" $
    [] @=? bcomp (And (Bc True) (And (Bc False) (Bc True))) True 3
  , testCase "Boolean Expressions" $
    [JMP 3] @=? bcomp (And (And (Bc True) (Bc True)) (And (Bc True) (Bc False))) False 3
  , testCase "Boolean Expressions" $
    [] @=? bcomp (And (And (Bc True) (Bc False)) (Less (V "x") (N 5))) True 3
  , testCase "Boolean Expressions" $
    [LOAD "x",LOADI 5,JMPGE 3] @=? bcomp (And (And (Bc True) (Bc True)) (Less (V "x") (N 5))) False 3
  , testCase "Boolean Expressions" $
    [LOAD "x",LOADI 3,JMPLESS 1,JMP 3,LOADI 2,LOAD "y",JMPLESS 3] @=? bcomp
                      (And (Less (V "x") (N 3)) (Less (N 2) (V "y"))) True 3
  , testCase "Boolean Expressions" $
    [LOAD "x",LOADI 2,JMPGE 7,JMP 3,LOAD "y",LOAD "z",JMPGE 3] @=? bcomp
                      (And (And (Bc True) (Less (V "x") (N 2))) (Less (V "y") (V "z"))) False 3
  ]
