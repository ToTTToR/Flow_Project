buildbyte : 
	ocamlbuild ftest.byte

buildnat : 
	ocamlbuild ftest.native

clean :
	rm -f *.cmi *.cmo ftest ftest.native ftest.byte

build : remove helper
	ocamlc -o ftest graph.cmo gfile.cmo Tools.cmo ftest.ml

helper :
	ocamlc -c graph.mli graph.ml gfile.mli gfile.ml Tools.mli Tools.ml