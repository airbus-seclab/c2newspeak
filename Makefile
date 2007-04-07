.PHONY = all clean test

all:

test:

clean:
	make clean -C src
	make clean -C tests
	make clean -C bin
	rm -f *~

tar: clean
	cd ..; tar -czf newspeak.tgz newspeak