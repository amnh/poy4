# $Id: Makefile 1819 2007-05-11 21:42:42Z andres $ 
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

mac_panther:
	cp -f config myconfig
	cp -f config.panther config
	$(MAKE) clean; $(MAKE) depend; $(MAKE) mpoy.opt
	cp -f myconfig config
	cp src/mpoy.opt ./panther/poy

mac_ppc:
	cp -f config myconfig
	cp -f config.ppc_seq config
	$(MAKE) clean; $(MAKE) depend; $(MAKE) mpoy.opt
	cp -f ./src/mpoy.opt ./universal_sequential/poy_ppc
	cp -f myconfig config

mac_ppc_parallel_ncurses:
	cp -f config myconfig
	cp -f config.ppc_par_ncurses config
	$(MAKE) ocamlmpi
	$(MAKE) clean; $(MAKE) depend; $(MAKE) mpoy.opt
	cp -f myconfig config

mac_ppc_parallel_flat:
	cp -f config myconfig
	cp -f config.ppc_par config
	$(MAKE) ocamlmpi
	$(MAKE) clean; $(MAKE) depend; $(MAKE) mpoy.opt
	cp -f myconfig config

mac_ppc_parallel:
	$(MAKE) mac_ppc_parallel_ncurses
	cp src/mpoy.opt ./universal_parallel/mppoy_ppc
	$(MAKE) mac_ppc_parallel_flat
	cp src/mpoy.opt ./universal_parallel/ppoy_ppc

mac_intel:
	cp -f config myconfig
	cp -f config.intel_seq config
	$(MAKE) clean; $(MAKE) depend; $(MAKE) mpoy.opt
	cp -f ./src/mpoy.opt ./universal_sequential/poy_intel
	cp -f myconfig config

mac_intel_parallel_ncurses:
	cp -f config myconfig
	cp -f config.intel_par_ncurses config
	$(MAKE) ocamlmpi
	$(MAKE) clean; $(MAKE) depend; $(MAKE) mpoy.opt
	cp -f myconfig config

mac_intel_parallel_flat:
	cp -f config myconfig
	cp -f config.intel_par config
	$(MAKE) ocamlmpi
	$(MAKE) clean; $(MAKE) depend; $(MAKE) mpoy.opt
	cp -f myconfig config

mac_intel_parallel:
	$(MAKE) mac_intel_parallel_ncurses
	cp src/mpoy.opt ./universal_parallel/mppoy_intel
	$(MAKE) mac_intel_parallel_flat
	cp src/mpoy.opt ./universal_parallel/ppoy_intel

mac_universal:
	$(MAKE) mac_intel
	$(MAKE) mac_ppc
	lipo -create ./universal_sequential/poy_ppc ./universal_sequential/poy_intel -output ./universal_sequential/poy

mac_universal_parallel:
	$(MAKE) mac_intel_parallel
	$(MAKE) mac_ppc_parallel
	lipo -create ./universal_parallel/mppoy_ppc ./universal_parallel/mppoy_intel -output ./universal_parallel/mppoy
	lipo -create ./universal_parallel/ppoy_ppc ./universal_parallel/ppoy_intel -output ./universal_parallel/ppoy

mac_all:
	$(MAKE) mac_panther
	$(MAKE) mac_universal_parallel
	$(MAKE) mac_universal


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

distro_directories:
	mkdir universal_parallel
	mkdir universal_sequential
	mkdir panther

clean_distro:
	rm -f universal_parallel/*
	rm -f universal_sequential/*
	rm -f panther/*

clean:
	@for subs in $(SUBDIRS); do \
	  cd $$subs; \
	  $(MAKE) clean; \
	  cd ..; \
	done
