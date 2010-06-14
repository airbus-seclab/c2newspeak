Newspeak output
---------------
219.c
int32 f(void) {
  (219.c:27#1064)^!return =(int32) 0;
}

void main(void) {
  (219.c:31#1097)^int32 x;
  int32 tmp;
  (219.c:32#1102)^tmp <- f();
  (219.c:32#1102)^x =(int32) coerce[-2147483648,2147483647] (tmp_int32 + 1);
  (219.c:33#1117)^x =(int32) 2;
}


