# commands
RM = rm -f
C2NEWSPEAK = ../../bin/c2newspeak --experimental -c

TESTS = 000

TESTS.SPEC = $(addsuffix .spec, $(TESTS))

.SILENT: check $(TESTS)
.PHONY: $(TESTS.SPEC)

check: $(TESTS)

$(TESTS): ../../bin/c2newspeak $@
	eval $(C2NEWSPEAK) $@.c &> /dev/null
	wc -c $@.no > $@.bak
	dos2unix $@.bak
	if [ -e $@.spec ]; \
	then \
	  if diff -w $@.spec $@.bak > result; \
            then true; \
            else cat result; false; \
	  fi; \
	else \
	  echo "$@.spec does not exist; printing output instead of diffing"; \
	  cat $@.bak; false; \
	fi;
	$(RM) $@.bak result
	touch $@

$(TESTS.SPEC): ../../bin/c2newspeak
	eval $(C2NEWSPEAK) $(basename $@).c
	wc -c $(basename $@).no > $@
	dos2unix $@
	cat $@

../../bin/c2newspeak: 
	make -C ../.. install

clean:
	$(RM) $(TESTS)
	$(RM) *~ *.bak result *.no *.npk

