Warning: incomplete prototype for function f in 540.c line 26
Warning: unknown arguments type at function call accepted in 540.c line 30
Newspeak output
---------------
540.c
f() {
}

main() {
  (540.c:29#7)^ptr x;
  (540.c:30#2)^ptr f.arg1;
  (540.c:30#2)^0- =(ptr) 1-_ptr;
  (540.c:30#2)^f();
}


