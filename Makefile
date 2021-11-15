build : remove
	ocamlbuild ftest.native

remove :
	rm -f *.cmi *.cmo ftest
