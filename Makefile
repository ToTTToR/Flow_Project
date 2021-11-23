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