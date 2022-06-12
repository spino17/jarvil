# JARVIL
Just A Rather Very Idiotic Language (JARVIL) is a programming language made purely out of fun! I just wanted to learn the answer of "How a prgramming language is made ?". This repo contains all my learnings on language design and writing a compiler. The patterns used in the implementation is heavily inspired by the famous dragon book on compilers (`Compilers: Principles, Techniques, and Tools, Second Edition. Alfred V. Aho, Monica S. Lam, Ravi Sethi, Jeffrey D. Ullman`). Because of the descriptive nature of this repo, it can be used by anyone who wishes to learn how to write a compiler for a simple language. All the stages for building the front-end of the compiler is implemented from scratch like lexical analyzer, parser, environments, type-checker, code-generator, semantic actions etc. For the backend, I have used llvm using the crate inkwell (safe wrapper on llvm-sys crate).

## Build Instruction
On Unix, Linux, BSD, macOS
`
./configure\n
make\n
sudo make install\n
`

## Formal Description
