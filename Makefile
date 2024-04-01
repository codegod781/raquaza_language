all : Rayquaza.out

##############################
#
#
# Compilation: 
# Option 1: Simply type "make" to compile Rayquaza Language (recommended, auto-test included)
# Option 2: "ocamlbuild Rayquaza.native" will also build the language

# For testing, you can run the binary executable and test it with
# standard input via terminal.
# Or use Rayquaza.tb (testbench file): you can modify the file directly
# with the exprssion you want to test before make. After compiling
# your executable successfully, the output of test case will be 
# generate automatically in a file named Rayquaza.out

Rayquaza.out : Rayquaza Rayquaza.tb
	./Rayquaza < Rayquaza.tb > Rayquaza.out

Rayquaza : Rayquaza_parser.cmo Rayquaza_scanner.cmo Rayquaza.cmo
	ocamlc -w A -o Rayquaza $^

%.cmo : %.ml
	ocamlc -w A -c $<

%.cmi : %.mli
	ocamlc -w A -c $<

Rayquaza_scanner.ml : Rayquaza_scanner.mll
	ocamllex $^

Rayquaza_parser.ml Rayquaza_parser.mli : Rayquaza_parser.mly
	ocamlyacc $^

# Update the rule for compiling Rayquaza_ast
Rayquaza_ast.cmi Rayquaza_ast.cmo : Rayquaza_ast.ml
	ocamlc -w A -c $<

# Update dependencies to use Rayquaza_ast.cmi and Rayquaza_ast.cmo
Rayquaza.cmo : Rayquaza_scanner.cmo Rayquaza_parser.cmi Rayquaza_ast.cmi
Rayquaza.cmx : Rayquaza_scanner.cmx Rayquaza_parser.cmx Rayquaza_ast.cmi
Rayquaza_parser.cmo : Rayquaza_ast.cmi
Rayquaza_parser.cmx : Rayquaza_ast.cmi
Rayquaza_scanner.cmo : Rayquaza_parser.cmi
Rayquaza_scanner.cmx : Rayquaza_parser.cmx

.PHONY : clean
clean :
	rm -rf *.cmi *.cmo Rayquaza_parser.ml Rayquaza_parser.mli Rayquaza_scanner.ml Rayquaza.out Rayquaza