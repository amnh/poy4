all: $(DEPEND) graphps.cmo graphps.cmx

.SUFFIXES: .ml .mli .cmo .cmx .cmi

$(DEPEND):	*.ml *.mli
	ocamldep -I $(INCLUDEOCAMLMPI) *.ml *.mli > $(DEPEND)

.ml.cmo:
	$(OCAMLC) -g -c $<

%.cmx: %.ml
	$(OCAMLOPT) -c $(CCOPT) $< $(PROFILE)

.ml.cmx:
	$(OCAMLC) -c $(CCOPT) $< $(PROFILE)

.mli.cmi:
	$(OCAMLC) -c -I $(INCLUDE) $(DEBUG) $<

.c.o:
	$(OCAMLOPT) $(CCOPT) -c $< $(PROFILE)


graphps.cmo: graphps.cmi graphps.ml

graphps.cmx: graphps.cmi graphps.ml

clean:	
	rm -f *.cmi *.cmo *.cmx 
