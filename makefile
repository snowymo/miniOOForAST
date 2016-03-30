all: delete
	ocamllex calculatorLEX.mll
	ocamlyacc calculatorYACC.mly
	ocamlc -c calculatorYACC.mli
	ocamlc -c calculatorLEX.ml
	ocamlc -c calculatorYACC.ml
	ocamlc -c calculator.ml
	@echo "# linking of the lexer, parser & calculator:"
	ocamlc -o calculator calculatorLEX.cmo calculatorYACC.cmo calculator.cmo
	ls
	@echo "# using the miniOO:"
	@echo "var r; var h; h=1; var p; p = proc y: r = y+h; var h; h=2; p(4);" | ./calculator
	@echo "# the end."

delete:
	/bin/rm -f calculator calculator.cmi calculator.cmo calculatorLEX.cmi calculatorLEX.cmo calculatorLEX.ml calculatorYACC.cmi calculatorYACC.cmo calculatorYACC.ml calculatorYACC.mli makefile~