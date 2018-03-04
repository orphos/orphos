#!/usr/bin/env bash
set -uxve
opam switch -y orphos --alias-of 4.06.0
eval $(opam config --shell=bash env)

# dependency packages
opam pin -y add ocamlfind 1.7.3-1
opam pin -y add num 1.1
opam pin -y add ocamlbuild 0.12.0
opam pin -y add batteries 2.8.0
opam pin -y add jbuilder 1.0+beta18
opam pin -y add menhir 20171222
opam pin -y add ounit 2.0.7

# development tools
opam pin -y add cppo 1.6.4
opam pin -y add easy-format 1.3.1
opam pin -y add biniou 1.2.0
opam pin -y add yojson 1.4.1
opam pin -y add merlin 3.0.5

opam pin -y add result 1.3
opam pin -y add topkg 0.9.1
opam pin -y add cmdliner 1.0.2
opam pin -y add ocp-build 1.99.20-beta
opam pin -y add ocp-indent 1.6.1

echo 'Run `eval $(opam config env)` before starting to develop ocaml-orphos!'

