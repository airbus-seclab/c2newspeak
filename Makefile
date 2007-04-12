.PHONY = all clean test

all:

test:

clean:
	make clean -C src
	make clean -C tests
	make clean -C bin
	make clean -C paper
	rm -f *~

tar: clean
	cd ..; tar -czf newspeak.tgz newspeak