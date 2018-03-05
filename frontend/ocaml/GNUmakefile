all: .DEFAULT
.DEFAULT:
	@which bmake > /dev/null 2>&1 || (echo "bmake is required for ocaml-orphos to build." && exit 1)
	@MAKEFLAGS=$(filter -j%,${MAKEFLAGS}) bmake -f BSDmakefile $(subst .DEFAULT, ,$@)