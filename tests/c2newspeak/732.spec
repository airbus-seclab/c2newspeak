Warning: 732.c:29#2: expression without post effects expected
Warning: 732.c:29#2: the order of execution of side-effects in expressions not specified, picking a random one, be careful
Newspeak output
---------------
void (732.c:26#5)^main(void) {
  (732.c:28#6)^int32 x;
  (732.c:29#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 - 1);
  (732.c:29#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 - 1);
  (732.c:29#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 - 1);
}


