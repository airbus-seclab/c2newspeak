Newspeak output
---------------
040.c
main() {
  (040.c:30#1132)^int32 e;
  (040.c:31#1150)^uint32 f;
  (040.c:33#1158)^0- =(uint32) coerce[0,4294967295] (coerce[0,4294967295] 1-_int32 + 0-_uint32);
  (040.c:34#1171)^0- =(uint32) coerce[0,4294967295] (0-_uint32 + coerce[0,4294967295] 1-_int32);
  (040.c:35#1184)^1- =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (coerce[0,4294967295] 1-_int32 + 0-_uint32);
  (040.c:36#1197)^1- =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (0-_uint32 + coerce[0,4294967295] 1-_int32);
}


