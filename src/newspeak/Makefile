# Project
DIR = 
OBJ = cilutils params npkcontext config newspeak \
      bare_csyntax csyntax synthack lexer parser \
      npkil npkutils cilenv \
      cilfirstpass cilcompiler \
      lstutils firstpass compiler \
      link c2newspeak

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
