Warning: 209.c:29#2: the order of execution of side-effects in expressions not specified, picking a random one, be careful
Newspeak output
---------------
209.c
void main(void) {
  (209.c:27#6)^int32 x;
  (209.c:27#9)^int32 y;
  (209.c:27#12)^int32 z;
  (209.c:29#2)^z =(int32) coerce[-2147483648,2147483647] (x_int32 + y_int32);
  (209.c:29#2)^y =(int32) coerce[-2147483648,2147483647] (y_int32 + 1);
  (209.c:29#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
}


