.PHONY: all clean test

all: test

clean:
	ocamlbuild -clean

%.native: FORCE
	ocamlbuild $@

FORCE:

test: mlorph.native
	./mlorph.native
