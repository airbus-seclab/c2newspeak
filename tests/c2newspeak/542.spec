Warning: 542.c:26#12: incomplete prototype for function f
Warning: 542.c:29#2: unknown arguments type at function call accepted
Newspeak output
---------------
542.c
void main(void) {
  (542.c:29#2)^int32 f.arg1;
  (542.c:29#2)^0- =(int32) 0;
  (542.c:29#2)^f();
}


