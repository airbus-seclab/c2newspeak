# Project
DIR = 
OBJ = cilutils newspeak npkcontext npkutils local_pass env cil2newspeak main

# interp


# Documentation
DOC_DIR = doc

# Test
TGT = cil2newspeak

include ../Makefile.common

machdep: machdep.c
	gcc -o machdep machdep.c

test: $(TGT)
	@cp $(TGT) ../../tests/newspeak/

include .depend
