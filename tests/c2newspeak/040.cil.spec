Newspeak output
---------------
040.c
void main(void) {
  (040.c:30#1132)^int32 e;
  (040.c:31#1150)^uint32 f;
  (040.c:33#1158)^f =(uint32) coerce[0,4294967295] (coerce[0,4294967295] e_int32 + f_uint32);
  (040.c:34#1171)^f =(uint32) coerce[0,4294967295] (f_uint32 + coerce[0,4294967295] e_int32);
  (040.c:35#1184)^e =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (coerce[0,4294967295] e_int32 + f_uint32);
  (040.c:36#1197)^e =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (f_uint32 + coerce[0,4294967295] e_int32);
}


