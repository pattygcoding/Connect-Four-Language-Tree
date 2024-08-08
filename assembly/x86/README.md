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
