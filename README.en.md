[한국어](./README.md) | [English](./README.en.md)

# billion row challenge

- A one-billion-row data processing challenge that you can attempt in various programming languages.

## How to participate

1. Run setup.sh to generate the test data. (Rust is required. The file is about 17GB.)
2. The input data file contains region/measurement pairs separated by newlines, as shown below. The numbers are integer values.

```
Prico;458634
Canada;780790
Central;798387
Athens;799643
Boa Vista;75486
```

3. Read that file and compute the maximum, minimum, count, sum, and average per region name.
4. When dividing, use integer arithmetic and discard the remainder.
5. Sort by region name in ascending order.
6. The expected output format is as follows: `region=min;max;average(sum/count)`

```
Adenarith=1;9999999;5000594(4716533623284/9431947)
Amsterdam=4;9999999;4999133(4715142248923/9431920)
Anápolis=0;9999999;4999550(4718363502090/9437576)
```

## Constraints

- It must run in a typical commodity computing environment.
  - linux/amd64, RAM 64GB, multi-core (12 cores)
- However, unsafe code and tricky black magic are allowed.
- Please name the folder or source file you submit so it is identifiable, in a form like nickname1 or nickname-1. As your number of attempts grows, increment the trailing number.

---

## Current results

| Rank | Lang                 | Code                                              | time      |
| ---- | -------------------- | ------------------------------------------------- | --------- |
| 1    | Rust                 | [whitetac(v3)](./rust/whitetac3//src/main.rs)     | 456ms     |
| 2    | Rust                 | [myyrakle](./rust/myyrakle1/src/main.rs)          | 486ms     |
| 3    | C++ (GCC)            | [miyamoto_renya](./cpp/miyamoto_renya/main.cpp)   | 853ms     |
| 4    | C++ (GCC)            | [libertyrapid](./cpp/libertyrapid1/main.cpp)      | 1176ms    |
| 5    | Rust                 | [s576air](./rust/s576air-1/src/main.rs)           | 1368ms    |
| 6    | Go                   | [lemon-mint(v2)](./go/cmd/lemon-mint2/main.go)    | 1434ms    |
| 7    | Go                   | [lemon-mint(v1)](./go/cmd/lemon-mint1/main.go)    | 1734ms    |
| 8    | Go (GCC)             | [lemon-mint(v3)](./go/cmd/lemon-mint3/main.go)    | 1829ms    |
| 9    | Rust                 | [whitetac(v2)](./rust/whitetac2/src/main.rs)      | 2148ms    |
| 10   | Rust                 | [whitetac(v1)](./rust/whitetac1/src/main.rs)      | 2187ms    |
| 11   | C#                   | [rudty](./csharp/rudty1/Program.cs)               | 2330ms    |
| 12   | Javascript (Node.js) | [kimseongjee](./javascript/kimseongjee/index.js)  | 10572ms   |
| 13   | Javascript (Node.js) | [sunrabbit(v2)](./javascript/sunrabbit2/index.js) | 20512ms   |
| 14   | C# (AOT)             | [Basic](./csharp/basic/Program.cs)                | 36356ms   |
| 15   | Kotlin (JVM)         | [Basic](./kotlin/basic/Main.kt)                   | 65369ms   |
| 16   | Go                   | [Basic](./go/cmd/basic/main.go)                   | 66842ms   |
| 17   | Rust                 | [Basic](./rust/basic/src/main.rs)                 | 74631ms   |
| 18   | C++ (GCC)            | [Basic](./cpp/basic/main.cpp)                     | 85803ms   |
| 19   | Java (ZGC)           | [Basic](./java/basic/Main.java)                   | 88577ms   |
| 20   | Java (ParallelGC)    | [Basic](./java/basic/Main.java)                   | 90484ms   |
| 21   | Java (G1GC)          | [Basic](./java/basic/Main.java)                   | 90667ms   |
| 22   | C++ (Clang)          | [Basic](./cpp/basic/main.cpp)                     | 93765ms   |
| 23   | Java (GraalVM)       | [Basic](./java/basic/Main.java)                   | 117744ms  |
| 24   | PHP                  | [Basic](./php/basic.php)                          | 119706ms  |
| 25   | Common LISP          | [Basic](./lisp/main.lisp)                         | 131219ms  |
| 26   | C#                   | [Basic](./csharp/basic/Program.cs)                | 131325ms  |
| 27   | Julia                | [Basic](./julia/basic.jl)                         | 131684ms  |
| 28   | Ocaml                | [Basic](./ocaml/basic.ml)                         | 136462ms  |
| 29   | Python (pypy)        | [Basic](./python/basic/main.py)                   | 191197ms  |
| 30   | Javascript (Node.js) | [sunrabbit(v1)](./javascript/sunrabbit1/index.js) | 218040ms  |
| 31   | Dart (Dart VM)       | [Basic](./dart/basic/main.dart)                   | 243129ms  |
| 32   | Pascal               | [Basic](./pascal/basic/main.pas)                  | 251556ms  |
| 33   | D                    | [Basic](./d/basic/main.d)                         | 260191ms  |
| 34   | Dart (AOT)           | [Basic](./dart/basic/main.dart)                   | 269486ms  |
| 35   | Javascript (Node.js) | [prravda1](./javascript/prravda1/index.js)        | 272899ms  |
| 36   | Kotlin (Native)      | [Basic](./kotlin/basic/MainNative.kt)             | 286757ms  |
| 37   | Javascript (Node.js) | [Basic](./javascript/basic.js)                    | 353595ms  |
| 38   | Javascript (Bun)     | [Basic](./javascript/basic.js)                    | 378794ms  |
| 39   | Python (CPython)     | [Basic](./python/basic/main.py)                   | 412043ms  |
| 40   | Perl                 | [Basic](./perl/basic.pl)                          | 530715ms  |
| 41   | Lua                  | [Basic](./lua/basic.lua)                          | 569031ms  |
| 42   | Ruby                 | [Basic](./ruby/basic.rb)                          | 643754ms  |
| 43   | Swift                | [Basic](./swift/basic.swift)                      | 936221ms  |
| 44   | Scheme               | [Basic](./scheme/basic.scm)                       | 967971ms  |
| 45   | Zig                  | [Basic](./zig/basic.zig)                          | 1017734ms |
| 46   | Erlang               | [Basic](./erlang/basic/main.erl)                  | 1787073ms |
| 47   | Haskell              | [Basic](./haskell/basic/Main.hs)                  | 3008400ms |
| 48   | R                    | [Basic](./r/basic/main.R)                         | 7655896ms |

---

## Environment per language

### C++

The version is G++ 16.1.1/Clang 21.1.6.

1. Create a new folder under the cpp path and set up your project.

### C#

The version is dotnet 9.0.110.

1. Create a new folder under the c# path and set up your project.
   - You may create it directly with dotnet new,
   - or copy basic and use that.

### D

The version is DMD v2.111.0.

1. Create a new folder under the d path and set up your project.

### Dart

The version is dart 3.10.0.

1. Create a new folder under the dart path and set up your project.

### Erlang

The version is V16.1.1.

1. Write a separate new folder and source file under the erlang path.

### Go

The version is Go 1.26.5.

1. Create a new folder under the go/cmd path.
2. Copy the go/cmd/basic/main.go file, then modify and optimize it.

### Haskell

The version is ghc 9.6.6.

1. Write a separate new folder and source file under the haskell path.

### Java

The versions are openjdk 25.0.1/graalvm 25.0.1.

1. Create a new folder under the java path and set up your project.

### Javascript

- The versions are Node.js v24.8.0/Bun v1.3.3.

1. Create a new file under the javascript path.
2. Copy the javascript/basic.js file, then modify and optimize it.

### Julia

The version is Julia 1.12.2.

1. Create a new folder under the julia path and set up your project.

### Kotlin

The versions are Kotlin 2.1.20, openjdk 25.0.1.

1. Create a new folder under the kotlin path and set up your project.

### LISP (Common LISP)

The version is SBCL v2.5.10.

1. Create a new file or a new folder under the lisp path and write your source code.

### LISP (Scheme)

The version is guile v3.0.10.

1. Create a new file or a new folder under the scheme path and write your source code.

### Lua

The version is lua 5.4.8.

1. Create a new folder under the lua path and set up your project.

### Ocaml

The version is v5.4.0.

1. Write a separate new folder and source file under the ocaml path.

### Pascal

The version is Free Pascal Compiler v3.2.2.

1. Create a new folder under the pascal path and set up your project.

### Perl

The version is v5.42.0.

1. Add a new folder and a new source file under the perl path.

### PHP

The version is 8.4.15.

1. Write a new source file under the php path.

### Python

The versions are CPython 3.14, pypy 3.11.13.
For CPython, uv is used.

1. Use uv to create a new folder and new file under the python path.

```bash
mkdir foo
cd foo
uv init
```

### R

The version is v4.5.2.

1. Create a new folder and file under the r path.

### Ruby

The version is Ruby 3.4.7.

1. Create a new file under the ruby path.

### Rust

The version is 1.97.1.

1. Set up an additional project under the rust path. You may copy basic, or create one with cargo new.

### Swift

The version is 6.2.1.

1. Create a new folder under the swift path and set up your project.

### Zig

The version is 0.15.2.

1. Create a new file or a new folder under the zig path and write your source code.

---

## Reference

- https://github.com/gunnarmorling/1brc
- https://benhoyt.com/writings/go-1brc/

## Past results

- [Season 1](./README.v1.md)
