DIR = src bin tests

.PHONY: $(DIR) all clean test install tar

install: src

all:

test:

$(DIR):
	$(MAKE) -C $@ $(MAKECMDGOALS)

clean: $(DIR)
	rm -f *~

tar:
	make clean
	cd ..; tar -czf c2newspeak.tgz c2newspeak-ref
