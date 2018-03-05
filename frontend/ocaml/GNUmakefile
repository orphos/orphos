.PHONY: all clean test

all: test

clean:
	ocamlbuild -clean

%.native:
	ocamlbuild $@

test: mlorphc.native
	./mlorphc.native
