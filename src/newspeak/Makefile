# Project
DIR = 
OBJ = lexer parser \
      newspeak cilutils params \
      npkcontext config npkil npkutils env \
      cilfirstpass cilcompiler compiler link c2newspeak

DEPEND = parser.mli parser.ml lexer.ml
CLEANFILES = $(DEPEND) parser.output

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

%.ml: %.mll
	ocamllex $<

%.mli %.ml: %.mly
	ocamlyacc -v $<

include .depend
