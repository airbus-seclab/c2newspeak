# Project
DIR = newspeak
OBJ = ../newspeak/newspeak \
      npkstats collector main

# Documentation
DOC_DIR = doc

# Test
TGT = npkstats

include ../Makefile.common

install: all
	@cp $(TGT) ../../bin/

test: $(TGT)
	@cp $(TGT) ../../tests/npkstats/$(TGT)


include .depend
