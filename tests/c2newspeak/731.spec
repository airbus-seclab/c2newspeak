Warning: 731.c:29#2: expression without post effects expected
Warning: 731.c:29#2: the order of execution of side-effects in expressions not specified, picking a random one, be careful
Newspeak output
---------------
731.c
void main(void) {
  (731.c:28#6)^int32 x;
  (731.c:29#2)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
  (731.c:29#2)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
  (731.c:29#2)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
}

