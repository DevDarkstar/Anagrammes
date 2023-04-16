OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt
INCLUDES=
OCAMLFLAGS=$(INCLUDES)
OCAMLOPTFLAGS=$(INCLUDES)

EXECUTABLES=anagrammes anagrammesx

all: $(EXECUTABLES)

################# pour compiler sans optimisation 
anagrammes: anagrammes.ml
	$(OCAMLC) -o anagrammes unix.cma anagrammes.ml


################# pour compiler avec optimisation 
anagrammesx: anagrammes.ml
	$(OCAMLOPT) -o anagrammesx unix.cmxa anagrammes.ml


################# clean up
clean:
	rm -f $(EXECUTABLES)
	rm -f *.cm[iox] *.o *.cma *.cmxa *.a *.cmi *.cmx