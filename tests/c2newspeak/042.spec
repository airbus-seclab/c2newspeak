Newspeak output
---------------
void (042.c:29#5)^main(void) {
  (042.c:30#7)^int8 a;
  (042.c:31#16)^uint8 b;
  (042.c:32#8)^int16 c;
  (042.c:33#17)^uint16 d;
  (042.c:34#6)^int32 e;
  (042.c:35#15)^uint32 f;
  (042.c:36#7)^int32 g;
  (042.c:37#16)^uint32 h;
  (042.c:39#2)^a =(int8) coerce[-128,127] (a_int8 + b_uint8);
  (042.c:40#2)^a =(int8) coerce[-128,127] (a_int8 + c_int16);
  (042.c:41#2)^a =(int8) coerce[-128,127] (a_int8 + d_uint16);
  (042.c:42#2)^a =(int8) coerce[-128,127] (a_int8 + e_int32);
  (042.c:43#2)^a =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] a_int8 + f_uint32);
  (042.c:44#2)^a =(int8) coerce[-128,127] (a_int8 + g_int32);
  (042.c:45#2)^a =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] a_int8 + h_uint32);
  (042.c:46#2)^a =(int8) coerce[-128,127] (b_uint8 + c_int16);
  (042.c:47#2)^a =(int8) coerce[-128,127] (b_uint8 + d_uint16);
  (042.c:48#2)^a =(int8) coerce[-128,127] (b_uint8 + e_int32);
  (042.c:49#2)^a =(int8) coerce[-128,127] coerce[0,4294967295] (b_uint8 + f_uint32);
  (042.c:50#2)^a =(int8) coerce[-128,127] (b_uint8 + g_int32);
  (042.c:51#2)^a =(int8) coerce[-128,127] coerce[0,4294967295] (b_uint8 + h_uint32);
  (042.c:52#2)^a =(int8) coerce[-128,127] (c_int16 + d_uint16);
  (042.c:53#2)^a =(int8) coerce[-128,127] (c_int16 + e_int32);
  (042.c:54#2)^a =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] c_int16 + f_uint32);
  (042.c:55#2)^a =(int8) coerce[-128,127] (c_int16 + g_int32);
  (042.c:56#2)^a =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] c_int16 + h_uint32);
  (042.c:57#2)^a =(int8) coerce[-128,127] (d_uint16 + e_int32);
  (042.c:58#2)^a =(int8) coerce[-128,127] coerce[0,4294967295] (d_uint16 + f_uint32);
  (042.c:59#2)^a =(int8) coerce[-128,127] (d_uint16 + g_int32);
  (042.c:60#2)^a =(int8) coerce[-128,127] coerce[0,4294967295] (d_uint16 + h_uint32);
  (042.c:61#2)^a =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] e_int32 + f_uint32);
  (042.c:62#2)^a =(int8) coerce[-128,127] (e_int32 + g_int32);
  (042.c:63#2)^a =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] e_int32 + h_uint32);
  (042.c:64#2)^a =(int8) coerce[-128,127] coerce[0,4294967295] (f_uint32 + coerce[0,4294967295] g_int32);
  (042.c:65#2)^a =(int8) coerce[-128,127] coerce[0,4294967295] (f_uint32 + h_uint32);
  (042.c:66#2)^a =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] g_int32 + h_uint32);
}


