Newspeak output
---------------
void (171.c:26#5)^main(void) {
  (171.c:27#15)^uint32 x;
  (171.c:28#15)^uint32 y;
  (171.c:29#6)^int32 z;
  (171.c:31#2)^y =(uint32) coerce[0,4294967295] (1 << 31);
  (171.c:32#2)^z =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (1 << 31);
  (171.c:33#2)^x =(uint32) 31;
  (171.c:34#2)^y =(uint32) coerce[0,4294967295] (1 << x_uint32);
  (171.c:35#2)^z =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (1 << x_uint32);
  (171.c:37#2)^y =(uint32) coerce[0,4294967295] (1 << 32);
  (171.c:38#2)^z =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (1 << 32);
  (171.c:39#2)^x =(uint32) 32;
  (171.c:40#2)^y =(uint32) coerce[0,4294967295] (1 << x_uint32);
  (171.c:41#2)^z =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (1 << x_uint32);
}


