UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=elf64
  NOPIE=-no-pie
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho64
  NOPIE=-Wl,-no_pie
endif
endif

compile: compile.ml
	ocamlfind ocamlc -o compile -thread -package oUnit -package sexplib -linkpkg -g compile.ml

%.run: %.o main.c
	clang $(NOPIE) -g -fsanitize=address -o $@ main.c $<

%.o: %.s
	nasm -f $(FORMAT) -o $@ $<

%.s: %.int compile
	./compile $< > $@
