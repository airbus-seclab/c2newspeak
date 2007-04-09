# Project
DIR = newspeak
OBJ = ../newspeak/params ../newspeak/cilutils ../newspeak/npkcontext \
      ../newspeak/newspeak \
      npkstats collector main

# Documentation
DOC_DIR = doc

# Test
TGT = npkstats

include ../Makefile.common

test: $(TGT)
	@cp $(TGT) ../../tests/npkstats/$(TGT)


include .depend
