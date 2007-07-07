# Project
DIR = 
OBJ = newspeak cilutils params \
      npkcontext npkil npkutils npkenv \
      npkfirstpass npkcompile npklink c2newspeak


# Documentation
DOC_DIR = doc

# Test
TGT = c2newspeak

install: all
	@cp $(TGT) ../../bin
	@cp newspeak.ml ../../bin
	@cp newspeak.mli ../../bin

include ../Makefile.common

machdep: machdep.c
	gcc -o machdep machdep.c

test: $(TGT)
	@cp $(TGT) ../../bin

include .depend
