# Makefile for gauche-rdf
# $Id: Makefile.in,v 0.1,2005/06/13 17:00:45$

PACKAGE_NAME      = gauche-rdf
PACKAGE_VERSION   = $(shell cat VERSION)
PACKAGE_FULL_NAME = $(PACKAGE_NAME)-$(PACKAGE_VERSION)

CONFIG_GENERATED = config.log config.status autom4te*.cache
.PHONY: all clean test check install distclean realclean dist

all: ; @ echo "% make install"

clean:
	rm -f core *~
	cd test; $(MAKE) clean

check:
	cd test; $(MAKE) check

install: all
	cd src; $(MAKE) install

distclean: clean
	rm -rf $(CONFIG_GENERATED)
	rm -f $(PACKAGE_FULL_NAME)*

dist:
	rm -f $(PACKAGE_FULL_NAME).tar.gz
	mkdir $(PACKAGE_FULL_NAME)
	cp -pRH $(filter-out $(PACKAGE_FULL_NAME), $(wildcard *)) $(PACKAGE_FULL_NAME)/
	tar zvcf $(PACKAGE_FULL_NAME).tar.gz $(PACKAGE_FULL_NAME)
	rm -rf $(PACKAGE_FULL_NAME)
