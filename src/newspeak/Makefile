# Project
DIR = 
OBJ = params newspeak cilutils \
      npkcontext npkil npkutils npkenv \
      npkfirstpass npkcompile npklink c2newspeak


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
	@cp $(TGT) ../../bin

include .depend
