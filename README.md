# Exec-Machine
Haskell based low-level execution machine with an interpreter and compiler.
It simulates an execution machine ([Machine.hs](https://github.com/ArastunM/Exec-Machine/blob/main/src/Machine.hs)), an
interpreter ([Interpreter.hs](https://github.com/ArastunM/Exec-Machine/blob/main/src/Interpreter.hs); containing arithmetic, boolean 
expressions and commands), and a compiler ([Compiler.hs](https://github.com/ArastunM/Exec-Machine/blob/main/src/Compiler.hs)) compiling commands
to be run on the execution machine.
The program was developed to be used with Haskell [Stack](https://docs.haskellstack.org/en/stable/README/) tool.

It consists of three folders:
- **[src]** - contains source files for the execution machine, interpreter and compiler.
- **[test]** - consisting of basic and advanced tests for all source.
- **[app]** - contains the main module invoked when the executable is executed.

## Installing Stack
Stack can be installed on most Unix-like (Un*x) operating systems, including macOS, and on Windows.
For Un*x OS, run:
```
  >> curl -sSL https://get.haskellstack.org/ | sh
```
For Windows users, install the [Windows 64-bit Installer](https://docs.haskellstack.org/en/stable/README/#how-to-install).

## Getting Started
The project can be initialised using the Stack tool (from within the project directory):
```
  >> stack ghci
```
To build and run the program run:
```
  >> stack build
  >> stack exec coursework-exe
```
Note that building will compile the project into **coursework-exe** 
executable, executing the **main** method from [Main.hs](https://github.com/ArastunM/Exec-Machine/blob/main/app/Main.hs).
For meaningful use of the program, pass in a parameter (command) before running the **coursework-exe**. For example:
```
  >> stack exec coursework-exe "Assign \"x\" (N 5)"
  [LOADI 5, STORE "x"]
```

To run the testcases (executing all tests from [test](https://github.com/ArastunM/Exec-Machine/blob/main/test) directory):
```
  >> stack test
```

## Details
- Author - Arastun Mammadli
- Completed as part of [ECM2418](http://emps.exeter.ac.uk/modules/ECM2418) coursework.
- License - [MIT](LICENSE)
