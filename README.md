# SKAN

## How to Compile and Run MicroC

Install all requirements using homebrew:

```brew install llvm```

```brew install pkg-config```

```opam install llvm```

Compile everything:

```ocamlbuild -pkgs llvm microc.native```

To compile and run example.mc:

```./microc.native -l example.mc > example.out```

```/usr/local/opt/llvm/bin/lli example.out```
