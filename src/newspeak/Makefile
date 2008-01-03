# Project
OBJ = cilutils params npkcontext config newspeak \
      pp_syntax pp_lexer pp_parser preprocess \
      bare_csyntax csyntax synthack lexer parser \
      npkil npkutils cilenv \
      cilfirstpass cilcompiler \
      lstutils firstpass compiler \
      link c2newspeak

DEPEND = pp_lexer.ml pp_parser.mli pp_parser.ml \
	 parser.mli parser.ml lexer.ml

CLEANFILES = $(DEPEND) pp_parser.output parser.output

# Executable
TGT = c2newspeak

INSTALL_FILES = newspeak.mli newspeak.ml

include ../Makefile.common

machdep: machdep.c
	gcc -o machdep machdep.c

include .depend
