# Project
DIR = newspeak
OBJ = ../newspeak/newspeak \
      npkstats collector main

# Documentation
DOC_DIR = doc

# Test
TGT = npkstats

install: all
	@cp $(TGT) ../../bin/

include ../Makefile.common

test: $(TGT)
	@cp $(TGT) ../../tests/npkstats/$(TGT)


include .depend
