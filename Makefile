DIR = src bin tests

.PHONY: $(DIR) all clean check install tar

install: src

check: 
	$(MAKE) -C tests

$(DIR):
	$(MAKE) -C $@ $(MAKECMDGOALS)

clean: $(DIR)
	rm -f *~

tar:
	make clean
	cd ..; tar -czf c2newspeak.tgz c2newspeak-ref
