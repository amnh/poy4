#CFLAGS=-I`$(OCAMLC) -where` -I$(MPIINCDIR) -O -g -Wall
-include ../config

COBJS=init.o comm.o msgs.o collcomm.o groups.o utils.o
OBJS=mpi.cmo

all: libcamlmpi.a mpi.cma mpi.cmxa 

install:
	cp mpi.mli mpi.cmi mpi.cma mpi.cmxa mpi.a libcamlmpi.a $(DESTDIRMPI)

libcamlmpi.a: $(COBJS)
	rm -f $@
	ar rc $@ $(COBJS)

mpi.cma: $(OBJS)
	$(OCAMLC) -a -o mpi.cma $(CCOPT) -custom $(OBJS) -cclib -lcamlmpi -ccopt -L$(MPILIBDIR) -cclib -l$(MPILNAME)

mpi.cmxa: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) -a -o mpi.cmxa $(CCOPT) $(OBJS:.cmo=.cmx) -cclib -lcamlmpi -ccopt -L$(MPILIBDIR) -cclib -l$(MPILNAME)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(CCOPT) -c $<
.mli.cmi:
	$(OCAMLC) $(CCOPT) -c $<
.ml.cmx:
	$(OCAMLOPT) $(CCOPT) -c $<
.c.o:
	$(OCAMLOPT) $(CCOPT) -c -I $(INCLUDE) $< $(PROFILE)

testmpi: test.ml mpi.cma libcamlmpi.a
	ocamlc -o testmpi unix.cma mpi.cma test.ml -ccopt -L.

clean::
	rm -f testmpi

test: testmpi
	mpirun -np 5 ./testmpi

test_mandel: test_mandel.ml mpi.cmxa libcamlmpi.a
	ocamlopt -o test_mandel graphics.cmxa mpi.cmxa test_mandel.ml -ccopt -L.

clean::
	rm -f test_mandel

clean::
	rm -f *.cm* *.o *.a
depend:
	$(OCAMLDEP) *.ml > .depend
	gcc -MM $(CFLAGS) *.c >> .depend

tol: 
	ocamlopt.opt -inline 500 -I native -c tol.ml
	cp tol.cmi ../
	cp tol.cmx ../
	cp tol.o ../

include .depend

