Warning: 'short' is not normalized: use 'short int' instead in 042.c line 32
Warning: 'short' is not normalized: use 'short int' instead in 042.c line 33
Warning: 'long' is not normalized: use 'long int' instead in 042.c line 36
Warning: 'long' is not normalized: use 'long int' instead in 042.c line 37
Newspeak output
---------------
042.c
main() {
  (042.c:30#1133)^int8;
  (042.c:31#1152)^uint8;
  (042.c:32#1163)^int16;
  (042.c:33#1183)^uint16;
  (042.c:34#1192)^int32;
  (042.c:35#1210)^uint32;
  (042.c:36#1220)^int32;
  (042.c:37#1239)^uint32;
  (042.c:39#1247)^7- =(int8) coerce[-128,127] (7-_int8 + 6-_uint8);
  (042.c:40#1260)^7- =(int8) coerce[-128,127] (7-_int8 + 5-_int16);
  (042.c:41#1273)^7- =(int8) coerce[-128,127] (7-_int8 + 4-_uint16);
  (042.c:42#1286)^7- =(int8) coerce[-128,127] (7-_int8 + 3-_int32);
  (042.c:43#1299)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] 7-_int8 + 2-_uint32);
  (042.c:44#1312)^7- =(int8) coerce[-128,127] (7-_int8 + 1-_int32);
  (042.c:45#1325)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] 7-_int8 + 0-_uint32);
  (042.c:46#1338)^7- =(int8) coerce[-128,127] (6-_uint8 + 5-_int16);
  (042.c:47#1351)^7- =(int8) coerce[-128,127] (6-_uint8 + 4-_uint16);
  (042.c:48#1364)^7- =(int8) coerce[-128,127] (6-_uint8 + 3-_int32);
  (042.c:49#1377)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (6-_uint8 + 2-_uint32);
  (042.c:50#1390)^7- =(int8) coerce[-128,127] (6-_uint8 + 1-_int32);
  (042.c:51#1403)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (6-_uint8 + 0-_uint32);
  (042.c:52#1416)^7- =(int8) coerce[-128,127] (5-_int16 + 4-_uint16);
  (042.c:53#1429)^7- =(int8) coerce[-128,127] (5-_int16 + 3-_int32);
  (042.c:54#1442)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] 5-_int16 + 2-_uint32);
  (042.c:55#1455)^7- =(int8) coerce[-128,127] (5-_int16 + 1-_int32);
  (042.c:56#1468)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] 5-_int16 + 0-_uint32);
  (042.c:57#1481)^7- =(int8) coerce[-128,127] (4-_uint16 + 3-_int32);
  (042.c:58#1494)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (4-_uint16 + 2-_uint32);
  (042.c:59#1507)^7- =(int8) coerce[-128,127] (4-_uint16 + 1-_int32);
  (042.c:60#1520)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (4-_uint16 + 0-_uint32);
  (042.c:61#1533)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] 3-_int32 + 2-_uint32);
  (042.c:62#1546)^7- =(int8) coerce[-128,127] (3-_int32 + 1-_int32);
  (042.c:63#1559)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] 3-_int32 + 0-_uint32);
  (042.c:64#1572)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (2-_uint32 + coerce[0,4294967295] 1-_int32);
  (042.c:65#1585)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (2-_uint32 + 0-_uint32);
  (042.c:66#1598)^7- =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] 1-_int32 + 0-_uint32);
}


