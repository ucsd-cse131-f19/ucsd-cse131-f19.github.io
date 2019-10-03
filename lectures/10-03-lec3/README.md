[Names & the Stack](https://course.ccs.neu.edu/cs4410/lec_let-and-stack_notes.html)
[All (we will say) About Parsing](http://papl.cs.brown.edu/2017/Processing_Programs__Parsing.html)
[Infix Operators](https://course.ccs.neu.edu/cs4410/lec_anf_notes.html)

The parsing link is not notes written with this course in mind, but is from a related programming languages textbook. It uses a programming language called Pyret for its examples, but the code should be recognizable. The cases expression in Pyret is similar to match in OCaml, and the data blocks are similar to type in OCaml. We will use a similar argument and s-expression parsing library for our work this quarter.

In the Infix Operators notes, Ben Lerner introduces an idea called A-Normal Form. We'll revisit this idea much later in the course; I'm going to not use it for the first few assignments and save it for when we need it for optimizations. In fact, we'll discuss in class the alternate approach suggested in those notes at (https://course.ccs.neu.edu/cs4410/lec_anf_notes.html#%28part._.An_alternate_approach__.Just_use_the_stack_%29).

Also, Ben Lerner's notes are for 32-bit x86 assembly. The only differences
that matter for us are that words are 8 bytes in our world, while they are 4
bytes in the notes, and the registers in the notes are called `esp` and `eax`
rather than `rsp` and `rax`.