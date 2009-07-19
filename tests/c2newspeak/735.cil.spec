Newspeak output
---------------
735.c
int32 f(void) {
  (735.c:27#1064)^0- =(int32) 1;
}

void main(void) {
  (735.c:31#1097)^int32 x;
  int32 tmp;
  (735.c:32#1102)^f();
  (735.c:32#1102)^1- =(int32) coerce[-2147483648,2147483647] (1 + 0-_int32);
}


