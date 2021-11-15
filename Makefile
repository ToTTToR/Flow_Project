buildbyte : remove
	ocamlbuild ftest.byte

buildnat : remove
	ocamlbuild ftest.native

remove :
	rm -f *.cmi *.cmo ftest

build : remove helper
	ocamlc -o ftest graph.cmo gfile.cmo ftest.ml

helper :
	ocamlc -c graph.mli graph.ml gfile.mli gfile.ml