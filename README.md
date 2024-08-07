# Connect Four Language Tree

## Instructions for running each program:

### Ada (ada):
```ada
gnatmake main.adb
./main
```

### x64 Assembly (assembly/x64):
```asm
nasm -f elf64 main.asm -o main.o
ld -s main.o -o main
./main
rm main main.o
```

### x86 Assembly (assembly/x86):
```asm
nasm -f elf32 main.asm -o main.o
ld -m elf_i386 main.o -o main
./main
rm main main.o
```

### C (c):
```c
gcc -o connect4 main.c
./connect4
```

### C with Windows (c/windows):
```c
gcc -o main.exe main.c -mwindows
./main.exe
```

### C# with .NET Core (c#/ConnectFour):
```csharp
dotnet build
dotnet run
```

### C# with .NET Core and Entity Framework (c#/ConnectFourEF):
```csharp
dotnet build
dotnet run
```

### C# with .NET Core and LINQ (c#/ConnectFourLinq):
```csharp
dotnet build
dotnet run
```

### C# with .NET Core and Windows Forms (c#/ConnectFourWPF):
```csharp
dotnet run
```

### C++ (c++):
```cpp
g++ -o connect4 main.cpp
./connect4
```

### C++ with Windows (c++/windows):
```cpp
g++ -o main.exe main.cpp -mwindows
./main.exe
```

### Dart (dart):
```dart
dart run main.dart
```

### Elixir (elixir):
```elixir
elixir main.exs
```

### F# with .NET Core (f#/ConnectFourWPF):
```fsharp
dotnet run
```

### Fortran (fortran):
```fortran
gfortran -o ConnectFour main.f90
./ConnectFour
```

### Golang (golang):
```go
go run main.go
```

### Groovy (groovy):
```groovy
groovy Main.groovy
```

### Haskell (haskell):
```haskell
ghc -o ConnectFour main.hs
./ConnectFour
```

### Java (java):
$ javac Main.java
$ java Main

### Java with SpringBoot (java/springboot):
Code documentation only, see the directory README for more information

### JavaScript with HTML5 & CSS3 (javascript/html-css):
$ python -m http.server
Then visit the link http://localhost:8000/index.html

### JavaScript with MySQL (javascript/mysql):
$ node main.js

### JavaScript with MongoDB & NoSQL (javascript/mongodb-nosql):
$ node main.js

### JavaScript with Next.js (javascript/next):
$ npm run dev
Then visit the link http://localhost:3000/main

### JavaScript with Node.js (javascript/node):
$ node main.js

### JavaScript with Vue.js (javascript/vue):
$ npm run serve
Then visit the link http://localhost:8080

### JavaScript with Vue.js using AI (javascript/vueai):
$ npm run serve
Then visit the link http://localhost:8080

### Kotlin (kotlin):
$ kotlinc main.kt -include-runtime -d main.jar
$ java -jar main.jar

### Lua (lua):
$ lua main.lua

### OCaml (ocaml)
$ ocamlc -o main main.ml
$ ./main

### Pascal (pascal):
$ fpc main.pas
$ ./main

### Perl (perl):
$ perl main.pl

### PHP (php):
$ php main.php

### PHP from Web Browser (php):
// WORK IN PROGRESS
$ php -S localhost:8000
Then visit http://localhost:8000/main.php

### PowerShell (powershell):
./ConnectFour/ps1

### Prolog (prolog):
$ swipl -q -s main.pl

### Python (python):
$ python main.py

### Python with JSON (python):
$ python main.py

### Python with tkinter (python/tkinter):
$ python main.py

### Ruby (ruby):
$ ruby main.rb

### Rust (rust):
$ cargo build
$ ./target/debug/connect-four
$ rm target/debug/connect-four

### Scala (scala):
$ scala main.scala

### Swift (swift):
$ swiftc main.swift -o main
$ ./main

### TypeScript with Angular (typescript/angular):
$ ng serve --open

### TypeScript with Node.js (typescript/node):
$ tsc main.ts
$ node main.js

### VB.NET Core (vbnet/ConnectFour)
$ dotnet run

### VB.NET Core with Windows Forms (vbnet/ConnectFourWPF)
$ dotnet run

### Bash (bash):
Work in progress
