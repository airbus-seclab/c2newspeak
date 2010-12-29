Newspeak output
---------------
void (040.c:29#5)^main(void) {
  (040.c:30#6)^int32 e;
  (040.c:31#15)^uint32 f;
  (040.c:33#2)^f =(uint32) coerce[0,4294967295] (coerce[0,4294967295] e_int32 + f_uint32);
  (040.c:34#2)^f =(uint32) coerce[0,4294967295] (f_uint32 + coerce[0,4294967295] e_int32);
  (040.c:35#2)^e =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (coerce[0,4294967295] e_int32 + f_uint32);
  (040.c:36#2)^e =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (f_uint32 + coerce[0,4294967295] e_int32);
}


