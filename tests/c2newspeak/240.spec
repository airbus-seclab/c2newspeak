Warning: 240.c:97#2: dirty cast from integer to pointer accepted
Warning: 240.c:109#2: dirty cast from integer to pointer accepted
Warning: 240.c:121#2: dirty cast from integer to pointer accepted
Warning: 240.c:133#2: dirty cast from integer to pointer accepted
Warning: 240.c:141#2: dirty cast from integer to pointer accepted
Warning: 240.c:142#2: dirty cast from integer to pointer accepted
Warning: 240.c:143#2: dirty cast from integer to pointer accepted
Warning: 240.c:144#2: dirty cast from integer to pointer accepted
Newspeak output
---------------
void main(void) {
  (240.c:27#7)^int8 a;
  (240.c:28#16)^uint8 b;
  (240.c:29#8)^int16 c;
  (240.c:30#17)^uint16 d;
  (240.c:31#6)^int32 e;
  (240.c:32#15)^uint32 f;
  (240.c:33#7)^int32 g;
  (240.c:34#16)^uint32 h;
  (240.c:35#7)^ptr i;
  (240.c:36#8)^float32 j;
  (240.c:37#9)^float64 k;
  (240.c:39#6)^int32 x;
  (240.c:41#2)^x =(int32) (a_int8 > a_int8);
  (240.c:42#2)^x =(int32) (a_int8 > b_uint8);
  (240.c:43#2)^x =(int32) (a_int8 > c_int16);
  (240.c:44#2)^x =(int32) (a_int8 > d_uint16);
  (240.c:45#2)^x =(int32) (a_int8 > e_int32);
  (240.c:46#2)^x =(int32) (coerce[0,4294967295] a_int8 > f_uint32);
  (240.c:47#2)^x =(int32) (a_int8 > g_int32);
  (240.c:48#2)^x =(int32) (coerce[0,4294967295] a_int8 > h_uint32);
  (240.c:50#2)^x =(int32) ((float32 <= int8) a_int8 > j_float32);
  (240.c:51#2)^x =(int32) ((float64 <= int8) a_int8 > k_float64);
  (240.c:53#2)^x =(int32) (b_uint8 > a_int8);
  (240.c:54#2)^x =(int32) (b_uint8 > b_uint8);
  (240.c:55#2)^x =(int32) (b_uint8 > c_int16);
  (240.c:56#2)^x =(int32) (b_uint8 > d_uint16);
  (240.c:57#2)^x =(int32) (b_uint8 > e_int32);
  (240.c:58#2)^x =(int32) (b_uint8 > f_uint32);
  (240.c:59#2)^x =(int32) (b_uint8 > g_int32);
  (240.c:60#2)^x =(int32) (b_uint8 > h_uint32);
  (240.c:62#2)^x =(int32) ((float32 <= uint8) b_uint8 > j_float32);
  (240.c:63#2)^x =(int32) ((float64 <= uint8) b_uint8 > k_float64);
  (240.c:65#2)^x =(int32) (c_int16 > a_int8);
  (240.c:66#2)^x =(int32) (c_int16 > b_uint8);
  (240.c:67#2)^x =(int32) (c_int16 > c_int16);
  (240.c:68#2)^x =(int32) (c_int16 > d_uint16);
  (240.c:69#2)^x =(int32) (c_int16 > e_int32);
  (240.c:70#2)^x =(int32) (coerce[0,4294967295] c_int16 > f_uint32);
  (240.c:71#2)^x =(int32) (c_int16 > g_int32);
  (240.c:72#2)^x =(int32) (coerce[0,4294967295] c_int16 > h_uint32);
  (240.c:74#2)^x =(int32) ((float32 <= int16) c_int16 > j_float32);
  (240.c:75#2)^x =(int32) ((float64 <= int16) c_int16 > k_float64);
  (240.c:77#2)^x =(int32) (d_uint16 > a_int8);
  (240.c:78#2)^x =(int32) (d_uint16 > b_uint8);
  (240.c:79#2)^x =(int32) (d_uint16 > c_int16);
  (240.c:80#2)^x =(int32) (d_uint16 > d_uint16);
  (240.c:81#2)^x =(int32) (d_uint16 > e_int32);
  (240.c:82#2)^x =(int32) (d_uint16 > f_uint32);
  (240.c:83#2)^x =(int32) (d_uint16 > g_int32);
  (240.c:84#2)^x =(int32) (d_uint16 > h_uint32);
  (240.c:86#2)^x =(int32) ((float32 <= uint16) d_uint16 > j_float32);
  (240.c:87#2)^x =(int32) ((float64 <= uint16) d_uint16 > k_float64);
  (240.c:89#2)^x =(int32) (e_int32 > a_int8);
  (240.c:90#2)^x =(int32) (e_int32 > b_uint8);
  (240.c:91#2)^x =(int32) (e_int32 > c_int16);
  (240.c:92#2)^x =(int32) (e_int32 > d_uint16);
  (240.c:93#2)^x =(int32) (e_int32 > e_int32);
  (240.c:94#2)^x =(int32) (coerce[0,4294967295] e_int32 > f_uint32);
  (240.c:95#2)^x =(int32) (e_int32 > g_int32);
  (240.c:96#2)^x =(int32) (coerce[0,4294967295] e_int32 > h_uint32);
  (240.c:97#2)^x =(int32) ((ptr) e_int32 > i_ptr);
  (240.c:98#2)^x =(int32) ((float32 <= int32) e_int32 > j_float32);
  (240.c:99#2)^x =(int32) ((float64 <= int32) e_int32 > k_float64);
  (240.c:101#2)^x =(int32) (f_uint32 > coerce[0,4294967295] a_int8);
  (240.c:102#2)^x =(int32) (f_uint32 > b_uint8);
  (240.c:103#2)^x =(int32) (f_uint32 > coerce[0,4294967295] c_int16);
  (240.c:104#2)^x =(int32) (f_uint32 > d_uint16);
  (240.c:105#2)^x =(int32) (f_uint32 > coerce[0,4294967295] e_int32);
  (240.c:106#2)^x =(int32) (f_uint32 > f_uint32);
  (240.c:107#2)^x =(int32) (f_uint32 > coerce[0,4294967295] g_int32);
  (240.c:108#2)^x =(int32) (f_uint32 > h_uint32);
  (240.c:109#2)^x =(int32) ((ptr) f_uint32 > i_ptr);
  (240.c:110#2)^x =(int32) ((float32 <= uint32) f_uint32 > j_float32);
  (240.c:111#2)^x =(int32) ((float64 <= uint32) f_uint32 > k_float64);
  (240.c:113#2)^x =(int32) (g_int32 > a_int8);
  (240.c:114#2)^x =(int32) (g_int32 > b_uint8);
  (240.c:115#2)^x =(int32) (g_int32 > c_int16);
  (240.c:116#2)^x =(int32) (g_int32 > d_uint16);
  (240.c:117#2)^x =(int32) (g_int32 > e_int32);
  (240.c:118#2)^x =(int32) (coerce[0,4294967295] g_int32 > f_uint32);
  (240.c:119#2)^x =(int32) (g_int32 > g_int32);
  (240.c:120#2)^x =(int32) (coerce[0,4294967295] g_int32 > h_uint32);
  (240.c:121#2)^x =(int32) ((ptr) g_int32 > i_ptr);
  (240.c:122#2)^x =(int32) ((float32 <= int32) g_int32 > j_float32);
  (240.c:123#2)^x =(int32) ((float64 <= int32) g_int32 > k_float64);
  (240.c:125#2)^x =(int32) (h_uint32 > coerce[0,4294967295] a_int8);
  (240.c:126#2)^x =(int32) (h_uint32 > b_uint8);
  (240.c:127#2)^x =(int32) (h_uint32 > coerce[0,4294967295] c_int16);
  (240.c:128#2)^x =(int32) (h_uint32 > d_uint16);
  (240.c:129#2)^x =(int32) (h_uint32 > coerce[0,4294967295] e_int32);
  (240.c:130#2)^x =(int32) (h_uint32 > f_uint32);
  (240.c:131#2)^x =(int32) (h_uint32 > coerce[0,4294967295] g_int32);
  (240.c:132#2)^x =(int32) (h_uint32 > h_uint32);
  (240.c:133#2)^x =(int32) ((ptr) h_uint32 > i_ptr);
  (240.c:134#2)^x =(int32) ((float32 <= uint32) h_uint32 > j_float32);
  (240.c:135#2)^x =(int32) ((float64 <= uint32) h_uint32 > k_float64);
  (240.c:141#2)^x =(int32) (i_ptr > (ptr) e_int32);
  (240.c:142#2)^x =(int32) (i_ptr > (ptr) f_uint32);
  (240.c:143#2)^x =(int32) (i_ptr > (ptr) g_int32);
  (240.c:144#2)^x =(int32) (i_ptr > (ptr) h_uint32);
  (240.c:145#2)^x =(int32) (i_ptr > i_ptr);
  (240.c:149#2)^x =(int32) (j_float32 > (float32 <= int8) a_int8);
  (240.c:150#2)^x =(int32) (j_float32 > (float32 <= uint8) b_uint8);
  (240.c:151#2)^x =(int32) (j_float32 > (float32 <= int16) c_int16);
  (240.c:152#2)^x =(int32) (j_float32 > (float32 <= uint16) d_uint16);
  (240.c:153#2)^x =(int32) (j_float32 > (float32 <= int32) e_int32);
  (240.c:154#2)^x =(int32) (j_float32 > (float32 <= uint32) f_uint32);
  (240.c:155#2)^x =(int32) (j_float32 > (float32 <= int32) g_int32);
  (240.c:156#2)^x =(int32) (j_float32 > (float32 <= uint32) h_uint32);
  (240.c:158#2)^x =(int32) (j_float32 > j_float32);
  (240.c:159#2)^x =(int32) ((float64 <= float32) j_float32 > k_float64);
  (240.c:161#2)^x =(int32) (k_float64 > (float64 <= int8) a_int8);
  (240.c:162#2)^x =(int32) (k_float64 > (float64 <= uint8) b_uint8);
  (240.c:163#2)^x =(int32) (k_float64 > (float64 <= int16) c_int16);
  (240.c:164#2)^x =(int32) (k_float64 > (float64 <= uint16) d_uint16);
  (240.c:165#2)^x =(int32) (k_float64 > (float64 <= int32) e_int32);
  (240.c:166#2)^x =(int32) (k_float64 > (float64 <= uint32) f_uint32);
  (240.c:167#2)^x =(int32) (k_float64 > (float64 <= int32) g_int32);
  (240.c:168#2)^x =(int32) (k_float64 > (float64 <= uint32) h_uint32);
  (240.c:170#2)^x =(int32) (k_float64 > (float64 <= float32) j_float32);
  (240.c:171#2)^x =(int32) (k_float64 > k_float64);
}


