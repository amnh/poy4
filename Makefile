# $Id: Makefile 1485 2007-01-04 23:08:52Z andres $ 
# Compilers
-include ./config

SUBDIRS = ocamlmpi src doc

.PHONY:	subdirs $(SUBDIRS) clean driver
subdirs:	$(SUBDIRS)

all: mpoy.opt mpoy mpoy_console
	$(MAKE) -C src

MAKEOCAMLMPI :=
ifeq ($(USEPARALLEL), true)
	MAKEOCAMLMPI := ocamlmpi
endif

binaries: mpoy-single mpoy-dual

mpoy-single: $(MAKEOCAMLMPI)
	./set-config.pl --ncurses --no-parallel
	cd src; $(MAKE) clean; $(MAKE) .depend; $(MAKE) mpoy.opt
	mv src/mpoy.opt $@

mpoy-dual: $(MAKEOCAMLMPI)
	./set-config.pl --no-ncurses --parallel
	cd src; $(MAKE) clean; $(MAKE) .depend; $(MAKE) mpoy.opt
	mv src/mpoy.opt $@

mpoy: $(MAKEOCAMLMPI)
	cd src; \
	$(MAKE) $@

mpoy.opt: $(MAKEOCAMLMPI)
	cd src; \
	$(MAKE) $@

mpoy_console: $(MAKEOCAMLMPI)
	cd src; \
	$(MAKE) $@

depend:
	cd src; \
	$(MAKE) $@

$(SUBDIRS):
	$(MAKE) -C $@

clean:
	@for subs in $(SUBDIRS); do \
	  cd $$subs; \
	  $(MAKE) clean; \
	  cd ..; \
	done
