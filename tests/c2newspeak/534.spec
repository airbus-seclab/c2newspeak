Warning: incomplete prototype for function h in 534.c line 26
Warning: incomplete prototype for function sort in 534.c line 29
Warning: unknown arguments type at function call accepted in 534.c line 32
Newspeak output
---------------
534.c
f() {
  (534.c:32#2)^fptr sort.arg1;
  (534.c:32#2)^0- =(fptr) &_{void -> void}(h);
  (534.c:32#2)^sort();
}

g() {
  (534.c:36#2)^fptr sort.arg1;
  (534.c:36#2)^0- =(fptr) &_{int32 -> void}(i);
  (534.c:36#2)^sort();
}


