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

Rayquaza.out : Rayquaza Rayquaza.tb
	./Rayquaza < Rayquaza.tb > Rayquaza.out

# Depedencies from ocamldep
Rayquaza.cmo : scanner.cmo parser.cmi ast.cmi
Rayquaza.cmx : scanner.cmx parser.cmx ast.cmi
Rayquaza_parser.cmo : ast.cmi parser.cmi
Rayquaza_parser.cmx : ast.cmi parser.cmi
Rayquaza_scanner.cmo : parser.cmi
Rayquaza_scanner.cmx : parser.cmx


##############################


.PHONY : clean
clean :
	rm -rf *.cmi *.cmo Rayquaza_parser.ml Rayquaza_parser.mli Rayquaza_scanner.ml Rayquaza.out Rayquaza
