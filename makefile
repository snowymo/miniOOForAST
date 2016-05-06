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
	@echo "# using the miniOO part 1:"
	@echo "var r; var h; h=1; var p; p = proc y: r = y+h; var h; h=2; p(4)#" | ./calculator
	@echo "# using the miniOO part 2:"
	@echo "var p; p = proc y:if y < 1 then p = 1 else p(y - 1); p(1)#" | ./calculator
	@echo "# using the miniOO part 3:"
	@echo "var x; malloc(x);x.C = 0;x.F = proc y:if y < 1 then x.R = x.C else x.F(y - 1);x.F(2)#" | ./calculator
	@echo "# using the miniOO part 4:"
	@echo "var x; malloc(x);x.C = 0;x.F = proc y:while x.C < 2 x.C = x.C + y; x.F(1)#" | ./calculator
	@echo "# using the miniOO part 5:"
	@echo "var x; malloc(x);x.C = 0;x.F = proc y:while x.C < 5 {x.C = x.C + y; x.C = x.C +1}; x.F(1)#" | ./calculator
	@echo "# using the miniOO part 6:"
	@echo "var r; var h; atom(h=1; h=2); h=3#" | ./calculator
	@echo "# using the miniOO part 7:"
	@echo "var r; var p; p = proc y : {r=3 ||| atom(r=y; r=2) }; p(1)#" | ./calculator
	@echo "# using the miniOO part 8:"
	@echo "var r; var p; var s; p = proc y : {r=3 ||| atom(r=y; r=2) }; p(1)#" | ./calculator
	@echo "# the end."

delete:
	/bin/rm -f calculator calculator.cmi calculator.cmo calculatorLEX.cmi calculatorLEX.cmo calculatorLEX.ml calculatorYACC.cmi calculatorYACC.cmo calculatorYACC.ml calculatorYACC.mli makefile~