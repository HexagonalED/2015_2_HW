OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLCFLAGS=
OCAMLOPTFLAGS=

COMPOBJS = location.cmx syntaxerr.cmx lexer.cmx parser.cmx abstract.cmx convert.cmx compare.cmx main.cmx
COMPCMIS = location.cmi syntaxerr.cmi parsetree.cmi abstract.cmi convert.cmi compare.cmi parser.cmi

TARGET = clonechecker

all: target

target: $(COMPCMIS) $(COMPOBJS)
	$(OCAMLOPT) -o $(TARGET) unix.cmxa $(COMPOBJS)

lexer.ml : lexer.mll
	$(OCAMLLEX) lexer.mll
parser.ml : parser.mly
	$(OCAMYACC) parser.mly
parser.mli : parser.mly	
	$(OCAMLYACC) parser.mly

.SUFFIXES: .cmo .cmi .ml .mli .mll .mly .cmx
.mli.cmi:
	$(OCAMLC) $(OCAMLCFLAGS) unix.cma -c $<
.mll.ml:
	$(OCAMLLEX) $<
.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) unix.cmxa -c $<

clean:
	rm *.cm[iox] *.o lexer.ml parser.ml parser.mli $(TARGET)
