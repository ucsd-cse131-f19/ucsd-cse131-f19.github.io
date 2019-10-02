compile: compile.ml
	ocamlfind ocamlc -o compile -thread -package oUnit -package sexplib -linkpkg -g compile.ml

%.run: %.o
	clang -o $@ main.c $<

%.o: %.s
	nasm -f macho64 -o $@ $<

%.s: %.int compile
	./compile $< > $@
