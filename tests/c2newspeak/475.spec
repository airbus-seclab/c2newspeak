Newspeak output
---------------
475.c
void f(int8) {
  (475.c:30#2)^0- =(int8) 2;
}

void main(void) {
  (475.c:34#6)^int32 c;
  (475.c:35#2)^int8 f.arg1;
  (475.c:35#2)^0- =(int8) coerce[-128,127] 1-_int32;
  (475.c:35#2)^f();
}


