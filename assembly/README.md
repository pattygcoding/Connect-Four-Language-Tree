# Connect Four - Assembly

Implementation of Connect Four game in Assembly language.

## Available Implementations

### x64 Assembly
**Prerequisite Installations:** 
- NASM
- GNU Binutils (LD)
- A 64-bit environment

**Commands:**
```bash
cd x64
nasm -f elf64 main.asm -o main.o
ld -s main.o -o main
./main
rm main main.o
```

### x86 Assembly
**Prerequisite Installations:** 
- NASM
- GNU Binutils (LD)
- A 32-bit environment

**Commands:**
```bash
cd x86
nasm -f elf32 main.asm -o main.o
ld -m elf_i386 main.o -o main
./main
rm main main.o
```

## About

This implementation demonstrates low-level programming concepts and direct hardware interaction through assembly language, showcasing the fundamental operations required for game logic at the processor instruction level.