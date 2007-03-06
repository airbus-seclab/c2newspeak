# Project
DIR = 
OBJ = cilutils npkcontext newspeak npkutils local_pass cil2newspeak main
# env
# OBJ = cilutils npkcontext newspeak npkutils npkfirstpass npkenv npkcompile cil2newspeak


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
