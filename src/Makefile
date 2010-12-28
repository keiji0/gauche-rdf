# Makefile for gauche-rdf

GOSH          = /usr/bin/gosh
GAUCHE_CONFIG = /usr/bin/gauche-config
INSTALL       = /usr/bin/install -c

SCM_INSTALL_DIR    = $(DESTDIR)`$(GAUCHE_CONFIG) --sitelibdir`
TARGET             = rdf/*.scm

.PHONY: all install clean

all: ; @echo "% make install"

install: 
	if test ! -d $(SCM_INSTALL_DIR)/rdf; then \
	  $(INSTALL) -d $(SCM_INSTALL_DIR)/rdf; \
	fi
	for f in $(TARGET); do \
	  $(INSTALL) -m 444 $$f $(SCM_INSTALL_DIR)/$$f; \
	done
