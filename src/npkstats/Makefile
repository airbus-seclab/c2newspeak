# Project
DIR = newspeak
OBJ = ../newspeak/cilutils ../newspeak/newspeak ../newspeak/npkcontext \
      ../newspeak/npkutils ../newspeak/local_pass ../newspeak/env \
      ../newspeak/cil2newspeak \
      npkstats collector main

# Documentation
DOC_DIR = doc

# Test
TGT = npkstats

include ../Makefile.common

test: $(TGT)
	@cp $(TGT) ../../tests/npkstats/$(TGT)


include .depend
