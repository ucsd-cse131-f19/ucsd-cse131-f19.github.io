compile: compile.ml
	ocamlfind ocamlc -o compile -thread -package oUnit -package sexplib -linkpkg -g compile.ml

output/%.run: output/%.o
	clang -o $@ -g -fsanitize=address main.c $<

output/%.o: output/%.s
	nasm -f macho64 -o $@ $<

output/%.s: input/%.int compile
	./compile $< > $@

