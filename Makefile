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
	python3 Showgraph.py