Warning: the order of execution of side-effects in expressions not specified, picking a random one, be careful in 209.c line 29
Newspeak output
---------------
209.c
main() {
  (209.c:27#1072)^int32;
  (209.c:27#1075)^int32;
  (209.c:27#1078)^int32;
  (209.c:29#1084)^0- =(int32) coerce[-2147483648,2147483647] (2-_int32 + 1-_int32);
  (209.c:29#1084)^2- =(int32) coerce[-2147483648,2147483647] (2-_int32 + 1);
  (209.c:29#1084)^1- =(int32) coerce[-2147483648,2147483647] (1-_int32 + 1);
}


