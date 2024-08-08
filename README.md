# Connect Four Language Tree by Patrick Goodwin

## Table of Contents
- [Ada](#ada-ada)
- Assembly
  - [x64](#x64-assembly-assemblyx64)
  - [x86](#x86-assembly-assemblyx86)
- [C](#c-c)
  - [Windows Library](#c-with-windows-cwindows)
- C#
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


## Instructions for running each program:

### Ada (ada):
```
gnatmake main.adb
./main
```

### x64 Assembly (assembly/x64):
```
nasm -f elf64 main.asm -o main.o
ld -s main.o -o main
./main
rm main main.o
```

### x86 Assembly (assembly/x86):
```
nasm -f elf32 main.asm -o main.o
ld -m elf_i386 main.o -o main
./main
rm main main.o
```

### C (c):
```
gcc -o connect4 main.c
./connect4
```

### C with Windows (c/windows):
```
gcc -o main.exe main.c -mwindows
./main.exe
```

### C# with .NET Core (c#/ConnectFour):
```
dotnet build
dotnet run
```

### C# with .NET Core and Entity Framework (c#/ConnectFourEF):
```
dotnet build
dotnet run
```

### C# with .NET Core and LINQ (c#/ConnectFourLinq):
```
dotnet build
dotnet run
```

### C# with .NET Core and Windows Forms (c#/ConnectFourWPF):
```
dotnet run
```

### C++ (c++):
```
g++ -o connect4 main.cpp
./connect4
```

### C++ with Windows (c++/windows):
```
g++ -o main.exe main.cpp -mwindows
./main.exe
```

### Dart (dart):
```
dart run main.dart
```

### Elixir (elixir):
```
elixir main.exs
```

### F# with .NET Core (f#/ConnectFourWPF):
```
dotnet run
```

### Fortran (fortran):
```
gfortran -o ConnectFour main.f90
./ConnectFour
```

### Golang (golang):
```
go run main.go
```

### Groovy (groovy):
```
groovy Main.groovy
```

### Haskell (haskell):
```
ghc -o ConnectFour main.hs
./ConnectFour
```

### Java (java):
```
javac Main.java
java Main
```

### Java with SpringBoot (java/springboot):
Code documentation only, see the directory README for more information

### JavaScript with HTML5 & CSS3 (javascript/html-css):
```
python -m http.server
```
Then visit the link http://localhost:8000/index.html

### JavaScript with MySQL (javascript/mysql):
```
node main.js
```

### JavaScript with MongoDB & NoSQL (javascript/mongodb-nosql):
```
node main.js
```

### JavaScript with Next.js (javascript/next):
```
npm run dev
```
Then visit the link http://localhost:3000/main

### JavaScript with Node.js (javascript/node):
```
node main.js
```

### JavaScript with Vue.js (javascript/vue):
```
npm run serve
```
Then visit the link http://localhost:8080

### JavaScript with Vue.js using AI (javascript/vueai):
```
npm run serve
```
Then visit the link http://localhost:8080

### Kotlin (kotlin):
```
kotlinc main.kt -include-runtime -d main.jar
java -jar main.jar
```

### Lua (lua):
```
lua main.lua
```

### OCaml (ocaml)
```
ocamlc -o main main.ml
./main
```

### Pascal (pascal):
```
fpc main.pas
./main
```

### Perl (perl):
```
perl main.pl
```

### PHP (php):
```
php main.php
```

### PHP from Web Browser (php):
WORK IN PROGRESS
```
php -S localhost:8000
```
Then visit http://localhost:8000/main.php

### PowerShell (powershell):
```
./ConnectFour/ps1
```

### Prolog (prolog):
```
swipl -q -s main.pl
```

### Python (python):
```
python main.py
```

### Python with JSON (python):
```
python main.py
```

### Python with tkinter (python/tkinter):
```
python main.py
```

### Ruby (ruby):
```
ruby main.rb
```

### Rust (rust):
```
cargo build
./target/debug/connect-four
rm target/debug/connect-four
```

### Scala (scala):
```
scala main.scala
```

### Swift (swift):
```
swiftc main.swift -o main
./main
```

### TypeScript with Angular (typescript/angular):
```
ng serve --open
```

### TypeScript with Node.js (typescript/node):
```
tsc main.ts
node main.js
```

### VB.NET Core (vbnet/ConnectFour)
```
dotnet run
```

### VB.NET Core with Windows Forms (vbnet/ConnectFourWPF)
```
dotnet run
```

### Bash (bash):
Work in progress
