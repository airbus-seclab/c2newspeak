.PHONY = all clean test

all:

test:

clean:
	make clean -C src
	make clean -C tests
	rm -f *~

tar: clean
	cd ..; tar -czf newspeak.tgz newspeak