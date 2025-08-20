# Connect Four - Go

[![Go](https://img.shields.io/badge/Go-00ADD8?style=for-the-badge&logo=go&logoColor=white)](https://golang.org/)

Implementation of Connect Four game in Go (Golang), a statically typed compiled language designed for simplicity and efficiency.

## Available Implementations

### Standard Go Implementation
**Prerequisite Installations:** 
- Go

**Command:**
```bash
go run main.go
```

### Fyne GUI Implementation (fyne)
**Prerequisite Installations:** 
- Go
- Fyne (GUI framework)

**Command:**
```bash
cd fyne
go run main.go
```

### WebAssembly Implementation (wasm)
**Prerequisite Installations:** 
- Go
- Web browser

**Commands:**
```bash
cd wasm
GOOS=js GOARCH=wasm go build -o main.wasm main.go
# Serve files and open in browser
```

## About

This collection demonstrates Go's versatility across different deployment targets:
- **Console**: Command-line interface with standard Go runtime
- **GUI**: Desktop applications with Fyne cross-platform framework
- **WebAssembly**: Browser-based applications compiled to WASM

Features showcased:
- Simple, clean syntax with powerful standard library
- Fast compilation and execution
- Built-in concurrency with goroutines and channels
- Strong typing with interface-based design
- Cross-platform deployment including web browsers
- Modern GUI development with native look and feel
