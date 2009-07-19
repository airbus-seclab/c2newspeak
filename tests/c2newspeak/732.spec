Warning: 732.c:29#2: expression without post effects expected
Warning: 732.c:29#2: the order of execution of side-effects in expressions not specified, picking a random one, be careful
Newspeak output
---------------
732.c
void main(void) {
  (732.c:28#6)^int32 x;
  (732.c:29#2)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 - 1);
  (732.c:29#2)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 - 1);
  (732.c:29#2)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 - 1);
}


