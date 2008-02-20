Warning: unnecessary creation of a pointer from a dereference: rewrite the code in 246.c line 29
Newspeak output
---------------
246.c
main() {
  (246.c:27#1073)^ptr;
  (246.c:29#1083)^0- =(ptr) &_32([0-_ptr]32);
}


