Warning: 240.c:97#2069: dirty cast from pointer to integer accepted
Warning: 240.c:109#2213: dirty cast from pointer to integer accepted
Warning: 240.c:121#2357: dirty cast from pointer to integer accepted
Warning: 240.c:133#2501: dirty cast from pointer to integer accepted
Warning: 240.c:141#2753: dirty cast from pointer to integer accepted
Warning: 240.c:142#2766: dirty cast from pointer to integer accepted
Warning: 240.c:143#2779: dirty cast from pointer to integer accepted
Warning: 240.c:144#2792: dirty cast from pointer to integer accepted
Warning: 240.c:145#2805: dirty cast from pointer to integer accepted
Warning: 240.c:145#2805: dirty cast from pointer to integer accepted
Newspeak output
---------------
240.c
void main(void) {
  (240.c:27#1073)^int8 a;
  (240.c:28#1092)^uint8 b;
  (240.c:29#1103)^int16 c;
  (240.c:30#1123)^uint16 d;
  (240.c:31#1132)^int32 e;
  (240.c:32#1150)^uint32 f;
  (240.c:33#1160)^int32 g;
  (240.c:34#1179)^uint32 h;
  (240.c:35#1187)^ptr i;
  (240.c:36#1200)^float32 j;
  (240.c:37#1212)^float64 k;
  (240.c:39#1222)^int32 x;
  (240.c:41#1230)^x =(int32) (a_int8 > a_int8);
  (240.c:42#1243)^x =(int32) (a_int8 > b_uint8);
  (240.c:43#1256)^x =(int32) (a_int8 > c_int16);
  (240.c:44#1269)^x =(int32) (a_int8 > d_uint16);
  (240.c:45#1282)^x =(int32) (a_int8 > e_int32);
  (240.c:46#1295)^x =(int32) (coerce[0,4294967295] a_int8 > f_uint32);
  (240.c:47#1308)^x =(int32) (a_int8 > g_int32);
  (240.c:48#1321)^x =(int32) (coerce[0,4294967295] a_int8 > h_uint32);
  (240.c:50#1386)^x =(int32) ((float32 <= int8) a_int8 > j_float32);
  (240.c:51#1399)^x =(int32) ((float64 <= int8) a_int8 > k_float64);
  (240.c:53#1413)^x =(int32) (b_uint8 > a_int8);
  (240.c:54#1426)^x =(int32) (b_uint8 > b_uint8);
  (240.c:55#1439)^x =(int32) (b_uint8 > c_int16);
  (240.c:56#1452)^x =(int32) (b_uint8 > d_uint16);
  (240.c:57#1465)^x =(int32) (b_uint8 > e_int32);
  (240.c:58#1478)^x =(int32) (b_uint8 > f_uint32);
  (240.c:59#1491)^x =(int32) (b_uint8 > g_int32);
  (240.c:60#1504)^x =(int32) (b_uint8 > h_uint32);
  (240.c:62#1570)^x =(int32) ((float32 <= uint8) b_uint8 > j_float32);
  (240.c:63#1583)^x =(int32) ((float64 <= uint8) b_uint8 > k_float64);
  (240.c:65#1597)^x =(int32) (c_int16 > a_int8);
  (240.c:66#1610)^x =(int32) (c_int16 > b_uint8);
  (240.c:67#1623)^x =(int32) (c_int16 > c_int16);
  (240.c:68#1636)^x =(int32) (c_int16 > d_uint16);
  (240.c:69#1649)^x =(int32) (c_int16 > e_int32);
  (240.c:70#1662)^x =(int32) (coerce[0,4294967295] c_int16 > f_uint32);
  (240.c:71#1675)^x =(int32) (c_int16 > g_int32);
  (240.c:72#1688)^x =(int32) (coerce[0,4294967295] c_int16 > h_uint32);
  (240.c:74#1754)^x =(int32) ((float32 <= int16) c_int16 > j_float32);
  (240.c:75#1767)^x =(int32) ((float64 <= int16) c_int16 > k_float64);
  (240.c:77#1781)^x =(int32) (d_uint16 > a_int8);
  (240.c:78#1794)^x =(int32) (d_uint16 > b_uint8);
  (240.c:79#1807)^x =(int32) (d_uint16 > c_int16);
  (240.c:80#1820)^x =(int32) (d_uint16 > d_uint16);
  (240.c:81#1833)^x =(int32) (d_uint16 > e_int32);
  (240.c:82#1846)^x =(int32) (d_uint16 > f_uint32);
  (240.c:83#1859)^x =(int32) (d_uint16 > g_int32);
  (240.c:84#1872)^x =(int32) (d_uint16 > h_uint32);
  (240.c:86#1938)^x =(int32) ((float32 <= uint16) d_uint16 > j_float32);
  (240.c:87#1951)^x =(int32) ((float64 <= uint16) d_uint16 > k_float64);
  (240.c:89#1965)^x =(int32) (e_int32 > a_int8);
  (240.c:90#1978)^x =(int32) (e_int32 > b_uint8);
  (240.c:91#1991)^x =(int32) (e_int32 > c_int16);
  (240.c:92#2004)^x =(int32) (e_int32 > d_uint16);
  (240.c:93#2017)^x =(int32) (e_int32 > e_int32);
  (240.c:94#2030)^x =(int32) (coerce[0,4294967295] e_int32 > f_uint32);
  (240.c:95#2043)^x =(int32) (e_int32 > g_int32);
  (240.c:96#2056)^x =(int32) (coerce[0,4294967295] e_int32 > h_uint32);
  (240.c:97#2069)^x =(int32) (coerce[0,4294967295] e_int32 > (uint32) i_ptr);
  (240.c:98#2082)^x =(int32) ((float32 <= int32) e_int32 > j_float32);
  (240.c:99#2095)^x =(int32) ((float64 <= int32) e_int32 > k_float64);
  (240.c:101#2109)^x =(int32) (f_uint32 > coerce[0,4294967295] a_int8);
  (240.c:102#2122)^x =(int32) (f_uint32 > b_uint8);
  (240.c:103#2135)^x =(int32) (f_uint32 > coerce[0,4294967295] c_int16);
  (240.c:104#2148)^x =(int32) (f_uint32 > d_uint16);
  (240.c:105#2161)^x =(int32) (f_uint32 > coerce[0,4294967295] e_int32);
  (240.c:106#2174)^x =(int32) (f_uint32 > f_uint32);
  (240.c:107#2187)^x =(int32) (f_uint32 > coerce[0,4294967295] g_int32);
  (240.c:108#2200)^x =(int32) (f_uint32 > h_uint32);
  (240.c:109#2213)^x =(int32) (f_uint32 > (uint32) i_ptr);
  (240.c:110#2226)^x =(int32) ((float32 <= uint32) f_uint32 > j_float32);
  (240.c:111#2239)^x =(int32) ((float64 <= uint32) f_uint32 > k_float64);
  (240.c:113#2253)^x =(int32) (g_int32 > a_int8);
  (240.c:114#2266)^x =(int32) (g_int32 > b_uint8);
  (240.c:115#2279)^x =(int32) (g_int32 > c_int16);
  (240.c:116#2292)^x =(int32) (g_int32 > d_uint16);
  (240.c:117#2305)^x =(int32) (g_int32 > e_int32);
  (240.c:118#2318)^x =(int32) (coerce[0,4294967295] g_int32 > f_uint32);
  (240.c:119#2331)^x =(int32) (g_int32 > g_int32);
  (240.c:120#2344)^x =(int32) (coerce[0,4294967295] g_int32 > h_uint32);
  (240.c:121#2357)^x =(int32) (coerce[0,4294967295] g_int32 > (uint32) i_ptr);
  (240.c:122#2370)^x =(int32) ((float32 <= int32) g_int32 > j_float32);
  (240.c:123#2383)^x =(int32) ((float64 <= int32) g_int32 > k_float64);
  (240.c:125#2397)^x =(int32) (h_uint32 > coerce[0,4294967295] a_int8);
  (240.c:126#2410)^x =(int32) (h_uint32 > b_uint8);
  (240.c:127#2423)^x =(int32) (h_uint32 > coerce[0,4294967295] c_int16);
  (240.c:128#2436)^x =(int32) (h_uint32 > d_uint16);
  (240.c:129#2449)^x =(int32) (h_uint32 > coerce[0,4294967295] e_int32);
  (240.c:130#2462)^x =(int32) (h_uint32 > f_uint32);
  (240.c:131#2475)^x =(int32) (h_uint32 > coerce[0,4294967295] g_int32);
  (240.c:132#2488)^x =(int32) (h_uint32 > h_uint32);
  (240.c:133#2501)^x =(int32) (h_uint32 > (uint32) i_ptr);
  (240.c:134#2514)^x =(int32) ((float32 <= uint32) h_uint32 > j_float32);
  (240.c:135#2527)^x =(int32) ((float64 <= uint32) h_uint32 > k_float64);
  (240.c:141#2753)^x =(int32) ((uint32) i_ptr > coerce[0,4294967295] e_int32);
  (240.c:142#2766)^x =(int32) ((uint32) i_ptr > f_uint32);
  (240.c:143#2779)^x =(int32) ((uint32) i_ptr > coerce[0,4294967295] g_int32);
  (240.c:144#2792)^x =(int32) ((uint32) i_ptr > h_uint32);
  (240.c:145#2805)^x =(int32) ((uint32) i_ptr > (uint32) i_ptr);
  (240.c:149#2895)^x =(int32) (j_float32 > (float32 <= int8) a_int8);
  (240.c:150#2908)^x =(int32) (j_float32 > (float32 <= uint8) b_uint8);
  (240.c:151#2921)^x =(int32) (j_float32 > (float32 <= int16) c_int16);
  (240.c:152#2934)^x =(int32) (j_float32 > (float32 <= uint16) d_uint16);
  (240.c:153#2947)^x =(int32) (j_float32 > (float32 <= int32) e_int32);
  (240.c:154#2960)^x =(int32) (j_float32 > (float32 <= uint32) f_uint32);
  (240.c:155#2973)^x =(int32) (j_float32 > (float32 <= int32) g_int32);
  (240.c:156#2986)^x =(int32) (j_float32 > (float32 <= uint32) h_uint32);
  (240.c:158#3037)^x =(int32) (j_float32 > j_float32);
  (240.c:159#3050)^x =(int32) ((float64 <= float32) j_float32 > k_float64);
  (240.c:161#3064)^x =(int32) (k_float64 > (float64 <= int8) a_int8);
  (240.c:162#3077)^x =(int32) (k_float64 > (float64 <= uint8) b_uint8);
  (240.c:163#3090)^x =(int32) (k_float64 > (float64 <= int16) c_int16);
  (240.c:164#3103)^x =(int32) (k_float64 > (float64 <= uint16) d_uint16);
  (240.c:165#3116)^x =(int32) (k_float64 > (float64 <= int32) e_int32);
  (240.c:166#3129)^x =(int32) (k_float64 > (float64 <= uint32) f_uint32);
  (240.c:167#3142)^x =(int32) (k_float64 > (float64 <= int32) g_int32);
  (240.c:168#3155)^x =(int32) (k_float64 > (float64 <= uint32) h_uint32);
  (240.c:170#3206)^x =(int32) (k_float64 > (float64 <= float32) j_float32);
  (240.c:171#3219)^x =(int32) (k_float64 > k_float64);
}


