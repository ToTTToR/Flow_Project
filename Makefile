buildbyte : remove
	ocamlbuild ftest.byte

buildnat : remove
	ocamlbuild ftest.native

remove :
	rm -f *.cmi *.cmo ftest ftest.native ftest.byte

build : remove helper
	ocamlc -o ftest graph.cmo gfile.cmo Tools.cmo ftest.ml 

helper :
	ocamlc -c graph.mli graph.ml gfile.mli gfile.ml Tools.mli Tools.ml