Newspeak output
---------------
054.c
void f(int8 x) {
  (054.c:30#1131)^x =(int8) 2;
}

void main(void) {
  (054.c:34#1161)^int32 c;
  (054.c:35#1166)^f(coerce[-128,127] c_int32);
}


