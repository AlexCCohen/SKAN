default: skan.native

skan.native:
	ocamlbuild skan.native -pkgs llvm

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -f *.native *.ir
	rm -f *.o *.a *.s *.out *.byte