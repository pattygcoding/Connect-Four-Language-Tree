# Connect Four Language Tree by Patrick Goodwin

## Table of Contents
- [Ada](#ada-ada)
- Assembly
  - [x64](#x64-assembly-assemblyx64)
  - [x86](#x86-assembly-assemblyx86)
- [C](#c-c)
  - [Windows Library](#c-with-windows-cwindows)
- [C#](#c-c)
  - [.NET Core](#c-with-net-core-cconnectfour)
  - [.NET Core and Entity Framework](#c-with-net-core-and-entity-framework-cconnectfouref)
  - [.NET Core and LINQ](#c-with-net-core-and-linq-cconnectfourlinq)
  - [.NET Core and Windows Forms](#c-with-net-core-and-windows-forms-cconnectfourwpf)
- [C++](#c-c)
  - [Windows Library](#c-with-windows-cwindows)
- [Dart](#dart-dart)
- [Elixir](#elixir-elixir)
- F#
  - [.NET Core](#f-with-net-core-fconnectfourwpf)
- [Fortran](#fortran-fortran)
- [Golang](#golang-golang)
- [Groovy](#groovy-groovy)
- [Haskell](#haskell-haskell)
- [Java](#java-java)
  - [SpringBoot](#java-with-springboot-javaspringboot)
- JavaScript
  - [HTML5 & CSS3](#javascript-with-html5--css3-javascripthtml-css)
  - [MySQL](#javascript-with-mysql-javascriptmysql)
  - [MongoDB & NoSQL](#javascript-with-mongodb--nosql-javascriptmongodb-nosql)
  - [Next.js](#javascript-with-nextjs-javascriptnext)
  - [Node.js](#javascript-with-nodejs-javascriptnode)
  - [Vue.js](#javascript-with-vuejs-javascriptvue)
  - [Vue.js using AI](#javascript-with-vuejs-using-ai-javascriptvueai)
- [Kotlin](#kotlin-kotlin)
- [Lua](#lua-lua)
- [OCaml](#ocaml-ocaml)
- [Pascal](#pascal-pascal)
- [Perl](#perl-perl)
- [PHP](#php-php)
  - [Web Browser](#php-from-web-browser-php)
- [PowerShell](#powershell-powershell)
- [Prolog](#prolog-prolog)
- [Python](#python-python)
  - [AI](#python-with-ai-python)
  - [JSON](#python-with-json-python)
  - [Tkinter](#python-with-tkinter-pythontkinter)
- [Ruby](#ruby-ruby)
- [Rust](#rust-rust)
- [Scala](#scala-scala)
- [Swift](#swift-swift)
- Typescript
  - [Angular](#typescript-with-angular-typescriptangular)
  - [Node](#typescript-with-nodejs-typescriptnode)
- VB.NET
  - [.NET Core](#vbnet-core-vbnetconnectfour)
  - [.NET Core with Windows Forms](#vbnet-core-with-windows-forms-vbnetconnectfourwpf)

## Ada (ada):
**Prerequisite Installations:** 
- GNAT (Part of GCC)

**Command:**
```
gnatmake main.adb
./main
```

## x64 Assembly (assembly/x64):
**Prerequisite Installations:** 
- NASM
- GNU Binutils (LD)
- A 64-bit environm4nt

**Command:**
```
nasm -f elf64 main.asm -o main.o
ld -s main.o -o main
./main
rm main main.o
```

## x86 Assembly (assembly/x86):
**Prerequisite Installations:** 
- NASM
- GNU Binutils (LD)
- A 32-bit environm4nt

**Command:**
```
nasm -f elf32 main.asm -o main.o
ld -m elf_i386 main.o -o main
./main
rm main main.o
```

## C (c):
**Prerequisite Installations:** 
- GCC

**Command:**
```
gcc -o connect4 main.c
./connect4
```

## C with Windows (c/windows):
**Prerequisite Installations:** 
- GCC

**Command:**
```
gcc -o main.exe main.c -mwindows
./main.exe
```

## C# (c#):
**Prerequisite Installations:** 
- CSC (Part of GCC)

**Command:**
```
csc main.cs
```

## C# with .NET Core (c#/ConnectFour):
**Prerequisite Installations:** 
- .NET Core SDK

**Command:**
```
dotnet build
dotnet run
```

## C# with .NET Core and Entity Framework (c#/ConnectFourEF):
**Prerequisite Installations:** 
- .NET Core SDK
- Entity Framework Core

**Command:**
```
dotnet build
dotnet run
```

## C# with .NET Core and LINQ (c#/ConnectFourLinq):
**Prerequisite Installations:** 
- .NET Core SDK

**Command:**
```
dotnet build
dotnet run
```

## C# with .NET Core and Windows Forms (c#/ConnectFourWPF):
**Prerequisite Installations:** 
- .NET Core SDK

**Command:**
```
dotnet run
```

## C++ (c++):
**Prerequisite Installations:** 
- G++ (Part of GCC)

**Command:**
```
g++ -o connect4 main.cpp
./connect4
```

## C++ with Windows (c++/windows):
**Prerequisite Installations:** 
- G++ (Part of GCC)

**Command:**
```
g++ -o main.exe main.cpp -mwindows
./main.exe
```

## Dart (dart):
**Prerequisite Installations:** 
- Dart SDK

**Command:**
```
dart run main.dart
```

## Elixir (elixir):
**Prerequisite Installations:** 
- Erlang
- Elixir

**Command:**
```
elixir main.exs
```

## F# with .NET Core (f#/ConnectFourWPF):
**Prerequisite Installations:** 
- .NET Core SDK

**Command:**
```
dotnet run
```

## Fortran (fortran):
**Prerequisite Installations:** 
- GFortran (part of GCC)

**Command:**
```
gfortran -o ConnectFour main.f90
./ConnectFour
```

## Golang (golang):
**Prerequisite Installations:** 
- Go

**Command:**
```
go run main.go
```

## Groovy (groovy):
**Prerequisite Installations:** 
- Java Development Kit (JDK)
- Groovy

**Command:**
```
groovy Main.groovy
```

## Haskell (haskell):
**Prerequisite Installations:** 
- GHC (Glasgow Haskell Compiler)

**Command:**
```
ghc -o ConnectFour main.hs
./ConnectFour
```

## Java (java):
**Prerequisite Installations:** 
- Java Development Kit (JDK)

**Command:**
```
javac Main.java
java Main
```

## Java with SpringBoot (java/springboot):
Code documentation only, see the directory README for more information

## JavaScript with HTML5 & CSS3 (javascript/html-css):
**Prerequisite Installations:** 
- Python (for HTTP server)

**Command:**
```
python -m http.server
```
Then visit the link http://localhost:8000/index.html

## JavaScript with MySQL (javascript/mysql):
**Prerequisite Installations:** 
- Node.js
- MySQL

**Command:**
```
node main.js
```

## JavaScript with MongoDB & NoSQL (javascript/mongodb-nosql):
**Prerequisite Installations:** 
- Node.js
- MongoDB

**Command:**
```
node main.js
```

## JavaScript with Next.js (javascript/next):
**Prerequisite Installations:** 
- Node.js
- Next.js

**Command:**
```
npm run dev
```
Then visit the link http://localhost:3000/main

## JavaScript with Node.js (javascript/node):
**Prerequisite Installations:** 
- Node.js

**Command:**
```
node main.js
```

## JavaScript with Vue.js (javascript/vue):
**Prerequisite Installations:** 
- Node.js
- Vue CLI

**Command:**
```
npm run serve
```
Then visit the link http://localhost:8080

## JavaScript with Vue.js using AI (javascript/vueai):
**Prerequisite Installations:** 
- Node.js
- Vue CLI

**Command:**
```
npm run serve
```
Then visit the link http://localhost:8080

## Kotlin (kotlin):
**Prerequisite Installations:** 
- Java Development Kit (JDK)
- Kotlin 

**Command:**
```
kotlinc main.kt -include-runtime -d main.jar
java -jar main.jar
```

## Lua (lua):
**Prerequisite Installations:** 
- Lua

**Command:**
```
lua main.lua
```

## OCaml (ocaml)
**Prerequisite Installations:** 
- OCAML Compiler

**Command:**
```
ocamlc -o main main.ml
./main
```

## Pascal (pascal):
**Prerequisite Installations:** 
- FPC (Free Pascal Compiler)

**Command:**
```
fpc main.pas
./main
```

## Perl (perl):
**Prerequisite Installations:** 
- Perl

**Command:**
```
perl main.pl
```

## PHP (php):
**Prerequisite Installations:** 
- PHP

**Command:**
```
php main.php
```

## PHP from Web Browser (php):
**Prerequisite Installations:** 
- PHP

**Command:**
WORK IN PROGRESS
```
php -S localhost:8000
```
Then visit http://localhost:8000/main.php

## PowerShell (powershell):
**Prerequisite Installations:** 
- Powershell

**Command:**
```
./ConnectFour/ps1
```

## Prolog (prolog):
**Prerequisite Installations:** 
- SWI-Prolog

**Command:**
```
swipl -q -s main.pl
```

## Python (python):
**Prerequisite Installations:** 
- Python

**Command:**
```
python main.py
```

## Python with AI (python/ai):
**Prerequisite Installations:** 
- Python

**Command:**
```
python main.py

## Python with JSON (python):
**Prerequisite Installations:** 
- Python

**Command:**
```
python main.py
```

## Python with Tkinter (python/tkinter):
**Prerequisite Installations:** 
- Python (Make sure Tkinter is included with your installation)

**Command:**
```
python main.py
```

## Ruby (ruby):
**Prerequisite Installations:** 
- Ruby

**Command:**
```
ruby main.rb
```

## Rust (rust):
**Prerequisite Installations:** 
- Rust Cargo

**Command:**
```
cargo build
./target/debug/connect-four
rm target/debug/connect-four
```

## Scala (scala):
**Prerequisite Installations:** 
- Scala

**Command:**
```
scala main.scala
```

## Swift (swift):
**Prerequisite Installations:** 
- Swift
- MacOS or a virtual env that can run Swift

**Command:**
```
swiftc main.swift -o main
./main
```

## TypeScript with Angular (typescript/angular):
**Prerequisite Installations:** 
- Node.js
- Angular CLI

**Command:**
```
ng serve --open
```

## TypeScript with Node.js (typescript/node):
**Prerequisite Installations:** 
- Node.js
- TypeScript

**Command:**
```
tsc main.ts
node main.js
```

## VB.NET Core (vbnet/ConnectFour)
**Prerequisite Installations:** 
- .NET Core SDK

**Command:**
```
dotnet run
```

## VB.NET Core with Windows Forms (vbnet/ConnectFourWPF)
**Prerequisite Installations:** 
- .NET Core SDK

**Command:**
```
dotnet run
```

## Bash (bash):
Work in progress
