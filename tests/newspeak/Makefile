TESTS=000 001 002 003 004 005
TESTS.SPEC = $(addsuffix .spec, $(TESTS))
EXEC=$(addsuffix _check, $(TESTS))

.SILENT: $(TESTS) $(EXEC) $(TESTS.SPEC)
.PHONY: $(TESTS.SPEC)
check: $(TESTS)

$(TESTS): %: %_check %.npk
	./$*_check $*.npk > $*.bak
	dos2unix $*.bak 2&> /dev/null; true
	if [ -e $*.spec ]; \
	then \
	  if diff $*.spec $*.bak > result; \
	  then true; \
	  else cat result; false; \
	  fi; \
	else \
	  echo "$*.spec does not exist; printing output instead of diffing"; \
	  cat $*.bak; false; \
	fi;
	touch $*
	echo $*

$(TESTS.SPEC): %.spec: %_check %.npk
	./$*_check $*.npk > $*.spec
	dos2unix $*.spec 2&> /dev/null; true
	cat $*.spec

$(EXEC): %_check: %.ml
	ocamlc -I ../../bin ../../bin/newspeak.cma $*.ml -o $*_check

%.npk: %.c
	@../../bin/c2newspeak $*.c -o $*.npk


clean:
	rm -f $(TESTS) 002.npk *.no
	rm -f $(EXEC) result
	rm -f *~ *.exe *.cmx *.cmo *.cmi *.o *.bak