all: expr evaluation miniml 

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte
	./miniml.byte

tests: expr_tests evaluation_tests

expr_tests: expr_tests.ml
	ocamlbuild -use-ocamlfind expr_tests.byte
	./expr_tests.byte
	
evaluation_tests: evaluation_tests.ml
	ocamlbuild -use-ocamlfind evaluation_tests.byte
	./evaluation_tests.byte
	
clean:
	rm -rf _build *.byte
