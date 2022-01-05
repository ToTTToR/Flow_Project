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
	@echo "\n==== RUNNING ====\n"
	./ftest.native $(in) $(s) $(p) pretty_graph
	dot -Tsvg init > init.svg
	open -a Safari init.svg
	dot -Tsvg pretty_graph > viz_graph2.svg
	open -a Safari viz_graph2.svg