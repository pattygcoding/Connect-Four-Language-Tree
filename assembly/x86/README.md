# Instructions for running program:

## x86 Assembly (assembly/x86):
```
nasm -f elf32 main.asm -o main.o
ld -m elf_i386 main.o -o main
./main
rm main main.o
```
