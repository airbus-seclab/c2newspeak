Newspeak output
---------------
042.c
main() {
  (042.c:30#7)^int8 a;
  (042.c:31#16)^uint8 b;
  (042.c:32#8)^int16 c;
  (042.c:33#17)^uint16 d;
  (042.c:34#6)^int32 e;
  (042.c:35#15)^uint32 f;
  (042.c:36#7)^int32 g;
  (042.c:37#16)^uint32 h;
  (042.c:39#2)^7- =(int8) coerce[-128,127] (7-_int8 + 6-_uint8);
  (042.c:40#2)^7- =(int8) coerce[-128,127] (7-_int8 + 5-_int16);
  (042.c:41#2)^7- =(int8) coerce[-128,127] (7-_int8 + 4-_uint16);
  (042.c:42#2)^7- =(int8) coerce[-128,127] (7-_int8 + 3-_int32);
  (042.c:43#2)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] 7-_int8 + 2-_uint32);
  (042.c:44#2)^7- =(int8) coerce[-128,127] (7-_int8 + 1-_int32);
  (042.c:45#2)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] 7-_int8 + 0-_uint32);
  (042.c:46#2)^7- =(int8) coerce[-128,127] (6-_uint8 + 5-_int16);
  (042.c:47#2)^7- =(int8) coerce[-128,127] (6-_uint8 + 4-_uint16);
  (042.c:48#2)^7- =(int8) coerce[-128,127] (6-_uint8 + 3-_int32);
  (042.c:49#2)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (6-_uint8 + 2-_uint32);
  (042.c:50#2)^7- =(int8) coerce[-128,127] (6-_uint8 + 1-_int32);
  (042.c:51#2)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (6-_uint8 + 0-_uint32);
  (042.c:52#2)^7- =(int8) coerce[-128,127] (5-_int16 + 4-_uint16);
  (042.c:53#2)^7- =(int8) coerce[-128,127] (5-_int16 + 3-_int32);
  (042.c:54#2)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] 5-_int16 + 2-_uint32);
  (042.c:55#2)^7- =(int8) coerce[-128,127] (5-_int16 + 1-_int32);
  (042.c:56#2)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] 5-_int16 + 0-_uint32);
  (042.c:57#2)^7- =(int8) coerce[-128,127] (4-_uint16 + 3-_int32);
  (042.c:58#2)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (4-_uint16 + 2-_uint32);
  (042.c:59#2)^7- =(int8) coerce[-128,127] (4-_uint16 + 1-_int32);
  (042.c:60#2)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (4-_uint16 + 0-_uint32);
  (042.c:61#2)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] 3-_int32 + 2-_uint32);
  (042.c:62#2)^7- =(int8) coerce[-128,127] (3-_int32 + 1-_int32);
  (042.c:63#2)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] 3-_int32 + 0-_uint32);
  (042.c:64#2)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (2-_uint32 + coerce[0,4294967295] 1-_int32);
  (042.c:65#2)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (2-_uint32 + 0-_uint32);
  (042.c:66#2)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] 1-_int32 + 0-_uint32);
}


