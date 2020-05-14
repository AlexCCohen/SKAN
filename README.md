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

To run the test suite:

```python tester.py```

`tester.py` is an automated tester that runs all of the programs in `tests`. Some of the programs are supposed to fail/throw errors and some are supposed to run successfully. The script checks each program accordingly.

There also tests for the different SKAN modules:

- ```parser_test.ml``` tests the scanner and parser
- ```infer_test.ml``` tests the type inference
- ```semantic_test.ml``` tests the semantic checker

### Demos

There are two demos in the `demos` folder.
