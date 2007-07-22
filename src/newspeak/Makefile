# Project
DIR = 
OBJ = newspeak cilutils params \
      npkcontext npkil npkutils env \
      firstpass npkcompile npklink c2newspeak


# Documentation
DOC_DIR = doc

# Executable
TGT = c2newspeak

INSTALL_FILES = newspeak.mli newspeak.ml

include ../Makefile.common

machdep: machdep.c
	gcc -o machdep machdep.c

test: $(TGT)
	@cp $(TGT) ../../bin

include .depend
