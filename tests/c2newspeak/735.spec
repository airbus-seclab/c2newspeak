Newspeak output
---------------
735.c
int32 f(void) {
  (735.c:27#2)^0- =(int32) 1;
}

void main(void) {
  (735.c:31#6)^int32 x;
  (735.c:32#2)^int32 !tmp0;
  (735.c:32#2)^f();
  (735.c:32#2)^1- =(int32) coerce[-2147483648,2147483647] (1 + 0-_int32);
}


