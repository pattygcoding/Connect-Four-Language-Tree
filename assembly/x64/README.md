# Instructions for running program:

## x64 Assembly (assembly/x64):
```
nasm -f elf64 main.asm -o main.o
ld -s main.o -o main
./main
rm main main.o
```
