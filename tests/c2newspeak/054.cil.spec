Newspeak output
---------------
054.c
void f(int8) {
  (054.c:30#1131)^0- =(int8) 2;
}

void main(void) {
  (054.c:34#1161)^int32 c;
  (054.c:35#1166)^int8 f.arg1;
  (054.c:35#1166)^0- =(int8) coerce[-128,127] 1-_int32;
  (054.c:35#1166)^f();
}


