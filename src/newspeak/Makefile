# Project
DIR = 
OBJ = cilutils npkcontext newspeak npkutils env \
      npkil local_pass cil2newspeak npklink main
# OBJ = cilutils npkcontext newspeak npkutils#  npkenv \
#      npkfirstpass npkcompile npklink cil2newspeak


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
