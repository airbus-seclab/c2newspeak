# Project
DIR = newspeak
OBJ = ../newspeak/params ../newspeak/cilutils ../newspeak/newspeak \
      ../newspeak/npkcontext ../newspeak/npkutils ../newspeak/npkenv \
      ../newspeak/npkil ../newspeak/npkfirstpass ../newspeak/npkcompile \
      ../newspeak/npklink \
      npkstats collector main

# Documentation
DOC_DIR = doc

# Test
TGT = npkstats

include ../Makefile.common

test: $(TGT)
	@cp $(TGT) ../../tests/npkstats/$(TGT)


include .depend
