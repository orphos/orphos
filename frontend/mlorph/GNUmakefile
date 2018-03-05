.PHONY: all clean test

all: test

clean:
	ocamlbuild -clean

%.native:
	ocamlbuild $@

test: mlorph.native
	./mlorph.native
