flipit: printer.cmo history.cmo main.cmo
	ocamlc unix.cma printer.cmo history.cmo main.cmo -o flipit

main.cmo: main.ml
	ocamlc -c main.ml

history.cmo: history.ml
	ocamlc -c history.ml

printer.cmo: printer.ml
	ocamlc -c printer.ml

clean:
	rm -f *.cmi *.cmo

