TESTS=000 001 002
TESTS.SPEC = $(addsuffix .spec, $(TESTS))
CHECK=$(addsuffix .checked, $(TESTS))

.SILENT: $(CHECK)

check: $(CHECK)

%.checked: read %.npk
	./read $*.npk > $*.bak
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
	touch $@
	echo $*

%.spec: read %.npk
	./read $*.npk > $*.spec
	dos2unix $*.spec 2&> /dev/null; true

002.npk:
	../../bin/c2newspeak 002.c -o 002.npk


$(TESTS.SPEC): BASE=$(basename $@)
$(TESTS.SPEC): read 
	./read $(BASE).npk > $(BASE).spec
	dos2unix $(BASE).spec 2&> /dev/null; true
	cat $(BASE).spec

read: read.ml
	ocamlc -I ../../bin ../../bin/newspeak.cma read.ml -o read

clean:
	rm -f $(CHECK) 002.npk *.no
	rm -f read result
	rm -f *~ *.exe *.cmx *.cmo *.cmi *.o *.bak