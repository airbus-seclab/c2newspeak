Newspeak output
---------------
475.c
void f(int8) {
  (475.c:30#1131)^0- =(int8) 2;
}

void main(void) {
  (475.c:34#1161)^int32 c;
  (475.c:35#1166)^int8 f.arg1;
  (475.c:35#1166)^0- =(int8) coerce[-128,127] 1-_int32;
  (475.c:35#1166)^f();
}


