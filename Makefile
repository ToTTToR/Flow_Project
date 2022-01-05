buildnat : 
	@echo "\n==== COMPILING ====\n"
	ocamlbuild ftest.native

format:
	ocp-indent --inplace src/*

edit:
	code . -n

clean :
	-rm -rf _build/
	-rm ftest.native
run : clean buildnat
	./ftest.native $(in) $(s) $(p) pretty_graph
	dot -Tsvg pretty_graph > vizgraph.svg
