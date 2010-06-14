Newspeak output
---------------
042.c
void main(void) {
  (042.c:30#1133)^int8 a;
  (042.c:31#1152)^uint8 b;
  (042.c:32#1163)^int16 c;
  (042.c:33#1183)^uint16 d;
  (042.c:34#1192)^int32 e;
  (042.c:35#1210)^uint32 f;
  (042.c:36#1220)^int32 g;
  (042.c:37#1239)^uint32 h;
  (042.c:39#1247)^a =(int8) coerce[-128,127] (a_int8 + b_uint8);
  (042.c:40#1260)^a =(int8) coerce[-128,127] (a_int8 + c_int16);
  (042.c:41#1273)^a =(int8) coerce[-128,127] (a_int8 + d_uint16);
  (042.c:42#1286)^a =(int8) coerce[-128,127] (a_int8 + e_int32);
  (042.c:43#1299)^a =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] a_int8 + f_uint32);
  (042.c:44#1312)^a =(int8) coerce[-128,127] (a_int8 + g_int32);
  (042.c:45#1325)^a =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] a_int8 + h_uint32);
  (042.c:46#1338)^a =(int8) coerce[-128,127] (b_uint8 + c_int16);
  (042.c:47#1351)^a =(int8) coerce[-128,127] (b_uint8 + d_uint16);
  (042.c:48#1364)^a =(int8) coerce[-128,127] (b_uint8 + e_int32);
  (042.c:49#1377)^a =(int8) coerce[-128,127] coerce[0,4294967295] (b_uint8 + f_uint32);
  (042.c:50#1390)^a =(int8) coerce[-128,127] (b_uint8 + g_int32);
  (042.c:51#1403)^a =(int8) coerce[-128,127] coerce[0,4294967295] (b_uint8 + h_uint32);
  (042.c:52#1416)^a =(int8) coerce[-128,127] (c_int16 + d_uint16);
  (042.c:53#1429)^a =(int8) coerce[-128,127] (c_int16 + e_int32);
  (042.c:54#1442)^a =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] c_int16 + f_uint32);
  (042.c:55#1455)^a =(int8) coerce[-128,127] (c_int16 + g_int32);
  (042.c:56#1468)^a =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] c_int16 + h_uint32);
  (042.c:57#1481)^a =(int8) coerce[-128,127] (d_uint16 + e_int32);
  (042.c:58#1494)^a =(int8) coerce[-128,127] coerce[0,4294967295] (d_uint16 + f_uint32);
  (042.c:59#1507)^a =(int8) coerce[-128,127] (d_uint16 + g_int32);
  (042.c:60#1520)^a =(int8) coerce[-128,127] coerce[0,4294967295] (d_uint16 + h_uint32);
  (042.c:61#1533)^a =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] e_int32 + f_uint32);
  (042.c:62#1546)^a =(int8) coerce[-128,127] (e_int32 + g_int32);
  (042.c:63#1559)^a =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] e_int32 + h_uint32);
  (042.c:64#1572)^a =(int8) coerce[-128,127] coerce[0,4294967295] (f_uint32 + coerce[0,4294967295] g_int32);
  (042.c:65#1585)^a =(int8) coerce[-128,127] coerce[0,4294967295] (f_uint32 + h_uint32);
  (042.c:66#1598)^a =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] g_int32 + h_uint32);
}


