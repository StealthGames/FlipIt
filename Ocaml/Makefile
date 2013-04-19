flipit: printer.cmx history.cmx main.cmx
	ocamlopt printer.cmx history.cmx main.cmx -o flipit

main.cmx: main.ml
	ocamlopt -c main.ml

history.cmx: history.ml
	ocamlopt -c history.ml

printer.cmx: printer.ml
	ocamlopt -c printer.ml

clean:
	rm -f *.cmi *.cmx *.o flipit

