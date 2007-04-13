DIR = src tests bin paper

.PHONY: $(DIR) all clean test install

install: src

all:

test:

$(DIR):
	$(MAKE) -C $@ $(MAKECMDGOALS)

clean: $(DIR)
	rm -f *~

tar: clean
	cd ..; tar -czf newspeak.tgz newspeak