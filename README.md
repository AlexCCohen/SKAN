# SKAN

## How to Compile and Run SKAN

Install all requirements using homebrew:

```brew install cmake```

```brew install llvm```

```brew install pkg-config```

```opam install llvm```

```brew install opencv@2```

### Run SKAN Programs

Compile:

```make```

To compile and run {BASE}.sk:

```./run.sh {BASE].sk```

```./{BASE}.out```


### Testing

To run the tests:

```python tester.py```

`tester.py` is an automated tester that runs all of the programs in `tests`. Some of the programs are supposed to fail/throw errors and some are supposed to run successfully. The script checks each program accordingly.

### Demos

There are two demos in the `demos` folder.
