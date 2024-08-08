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
