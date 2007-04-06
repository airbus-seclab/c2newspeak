# Project
DIR = 
OBJ = params cilutils \
      newspeak npkcontext npkutils npkenv \
      npkil npkfirstpass npkcompile npklink c2newspeak


# Documentation
DOC_DIR = doc

# Test
TGT = c2newspeak

include ../Makefile.common

machdep: machdep.c
	gcc -o machdep machdep.c

test: $(TGT)
	@cp $(TGT) ../../tests/newspeak/
	@cp $(TGT) ../../tests/multi-source/

include .depend
