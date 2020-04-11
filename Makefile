
#CXX=g++
#CXXFLAGS = -g -Wall
#CXXFLAGS += `pkg-config --cflags --libs opencv` -std=c++11
#LDFLAGS= -g

default: skan.native

skan.native:
	ocamlbuild skan.native -pkgs llvm

#libutils.a: utils.o
	#ar -crs libutils.a utils.o
	#ranlib libutils.a

#utils.o: utils.cpp
	#$(CXX) $(CXXFLAGS) utils.cpp -o utils.o

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -f *.native
	rm -f *.o *.a *.s a.out *.byte