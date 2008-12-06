Warning: 209.c:29#2: the order of execution of side-effects in expressions not specified, picking a random one, be careful
Newspeak output
---------------
209.c
void main(void) {
  (209.c:27#6)^int32 x;
  (209.c:27#9)^int32 y;
  (209.c:27#12)^int32 z;
  (209.c:29#2)^0- =(int32) coerce[-2147483648,2147483647] (2-_int32 + 1-_int32);
  (209.c:29#2)^2- =(int32) coerce[-2147483648,2147483647] (2-_int32 + 1);
  (209.c:29#2)^1- =(int32) coerce[-2147483648,2147483647] (1-_int32 + 1);
}


