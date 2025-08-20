# Connect Four - Erlang

Implementation of Connect Four game in Erlang, a functional programming language designed for fault-tolerant systems.

## Running the Game

**Prerequisite Installations:** 
- Erlang/OTP

**Command:**
```bash
erl -compile main
erl -noshell -s main start -s init stop
```

## About

This implementation demonstrates Erlang's unique features for building robust, concurrent applications:
- Actor model with lightweight processes
- Pattern matching
- Fault tolerance and supervision trees
- Distributed computing capabilities
- Hot code swapping
- Functional programming paradigms

Perfect for showcasing how game logic can be implemented in a language designed for high-availability systems.