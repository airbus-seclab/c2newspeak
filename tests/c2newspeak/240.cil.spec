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
main() {
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
  (240.c:41#1230)^0- =(int32) (11-_int8 > 11-_int8);
  (240.c:42#1243)^0- =(int32) (11-_int8 > 10-_uint8);
  (240.c:43#1256)^0- =(int32) (11-_int8 > 9-_int16);
  (240.c:44#1269)^0- =(int32) (11-_int8 > 8-_uint16);
  (240.c:45#1282)^0- =(int32) (11-_int8 > 7-_int32);
  (240.c:46#1295)^0- =(int32) (coerce[0,4294967295] 11-_int8 > 6-_uint32);
  (240.c:47#1308)^0- =(int32) (11-_int8 > 5-_int32);
  (240.c:48#1321)^0- =(int32) (coerce[0,4294967295] 11-_int8 > 4-_uint32);
  (240.c:50#1386)^0- =(int32) ((float32 <= int8) 11-_int8 > 2-_float32);
  (240.c:51#1399)^0- =(int32) ((float64 <= int8) 11-_int8 > 1-_float64);
  (240.c:53#1413)^0- =(int32) (10-_uint8 > 11-_int8);
  (240.c:54#1426)^0- =(int32) (10-_uint8 > 10-_uint8);
  (240.c:55#1439)^0- =(int32) (10-_uint8 > 9-_int16);
  (240.c:56#1452)^0- =(int32) (10-_uint8 > 8-_uint16);
  (240.c:57#1465)^0- =(int32) (10-_uint8 > 7-_int32);
  (240.c:58#1478)^0- =(int32) (10-_uint8 > 6-_uint32);
  (240.c:59#1491)^0- =(int32) (10-_uint8 > 5-_int32);
  (240.c:60#1504)^0- =(int32) (10-_uint8 > 4-_uint32);
  (240.c:62#1570)^0- =(int32) ((float32 <= uint8) 10-_uint8 > 2-_float32);
  (240.c:63#1583)^0- =(int32) ((float64 <= uint8) 10-_uint8 > 1-_float64);
  (240.c:65#1597)^0- =(int32) (9-_int16 > 11-_int8);
  (240.c:66#1610)^0- =(int32) (9-_int16 > 10-_uint8);
  (240.c:67#1623)^0- =(int32) (9-_int16 > 9-_int16);
  (240.c:68#1636)^0- =(int32) (9-_int16 > 8-_uint16);
  (240.c:69#1649)^0- =(int32) (9-_int16 > 7-_int32);
  (240.c:70#1662)^0- =(int32) (coerce[0,4294967295] 9-_int16 > 6-_uint32);
  (240.c:71#1675)^0- =(int32) (9-_int16 > 5-_int32);
  (240.c:72#1688)^0- =(int32) (coerce[0,4294967295] 9-_int16 > 4-_uint32);
  (240.c:74#1754)^0- =(int32) ((float32 <= int16) 9-_int16 > 2-_float32);
  (240.c:75#1767)^0- =(int32) ((float64 <= int16) 9-_int16 > 1-_float64);
  (240.c:77#1781)^0- =(int32) (8-_uint16 > 11-_int8);
  (240.c:78#1794)^0- =(int32) (8-_uint16 > 10-_uint8);
  (240.c:79#1807)^0- =(int32) (8-_uint16 > 9-_int16);
  (240.c:80#1820)^0- =(int32) (8-_uint16 > 8-_uint16);
  (240.c:81#1833)^0- =(int32) (8-_uint16 > 7-_int32);
  (240.c:82#1846)^0- =(int32) (8-_uint16 > 6-_uint32);
  (240.c:83#1859)^0- =(int32) (8-_uint16 > 5-_int32);
  (240.c:84#1872)^0- =(int32) (8-_uint16 > 4-_uint32);
  (240.c:86#1938)^0- =(int32) ((float32 <= uint16) 8-_uint16 > 2-_float32);
  (240.c:87#1951)^0- =(int32) ((float64 <= uint16) 8-_uint16 > 1-_float64);
  (240.c:89#1965)^0- =(int32) (7-_int32 > 11-_int8);
  (240.c:90#1978)^0- =(int32) (7-_int32 > 10-_uint8);
  (240.c:91#1991)^0- =(int32) (7-_int32 > 9-_int16);
  (240.c:92#2004)^0- =(int32) (7-_int32 > 8-_uint16);
  (240.c:93#2017)^0- =(int32) (7-_int32 > 7-_int32);
  (240.c:94#2030)^0- =(int32) (coerce[0,4294967295] 7-_int32 > 6-_uint32);
  (240.c:95#2043)^0- =(int32) (7-_int32 > 5-_int32);
  (240.c:96#2056)^0- =(int32) (coerce[0,4294967295] 7-_int32 > 4-_uint32);
  (240.c:97#2069)^0- =(int32) (coerce[0,4294967295] 7-_int32 > (uint32) 3-_ptr);
  (240.c:98#2082)^0- =(int32) ((float32 <= int32) 7-_int32 > 2-_float32);
  (240.c:99#2095)^0- =(int32) ((float64 <= int32) 7-_int32 > 1-_float64);
  (240.c:101#2109)^0- =(int32) (6-_uint32 > coerce[0,4294967295] 11-_int8);
  (240.c:102#2122)^0- =(int32) (6-_uint32 > 10-_uint8);
  (240.c:103#2135)^0- =(int32) (6-_uint32 > coerce[0,4294967295] 9-_int16);
  (240.c:104#2148)^0- =(int32) (6-_uint32 > 8-_uint16);
  (240.c:105#2161)^0- =(int32) (6-_uint32 > coerce[0,4294967295] 7-_int32);
  (240.c:106#2174)^0- =(int32) (6-_uint32 > 6-_uint32);
  (240.c:107#2187)^0- =(int32) (6-_uint32 > coerce[0,4294967295] 5-_int32);
  (240.c:108#2200)^0- =(int32) (6-_uint32 > 4-_uint32);
  (240.c:109#2213)^0- =(int32) (6-_uint32 > (uint32) 3-_ptr);
  (240.c:110#2226)^0- =(int32) ((float32 <= uint32) 6-_uint32 > 2-_float32);
  (240.c:111#2239)^0- =(int32) ((float64 <= uint32) 6-_uint32 > 1-_float64);
  (240.c:113#2253)^0- =(int32) (5-_int32 > 11-_int8);
  (240.c:114#2266)^0- =(int32) (5-_int32 > 10-_uint8);
  (240.c:115#2279)^0- =(int32) (5-_int32 > 9-_int16);
  (240.c:116#2292)^0- =(int32) (5-_int32 > 8-_uint16);
  (240.c:117#2305)^0- =(int32) (5-_int32 > 7-_int32);
  (240.c:118#2318)^0- =(int32) (coerce[0,4294967295] 5-_int32 > 6-_uint32);
  (240.c:119#2331)^0- =(int32) (5-_int32 > 5-_int32);
  (240.c:120#2344)^0- =(int32) (coerce[0,4294967295] 5-_int32 > 4-_uint32);
  (240.c:121#2357)^0- =(int32) (coerce[0,4294967295] 5-_int32 > (uint32) 3-_ptr);
  (240.c:122#2370)^0- =(int32) ((float32 <= int32) 5-_int32 > 2-_float32);
  (240.c:123#2383)^0- =(int32) ((float64 <= int32) 5-_int32 > 1-_float64);
  (240.c:125#2397)^0- =(int32) (4-_uint32 > coerce[0,4294967295] 11-_int8);
  (240.c:126#2410)^0- =(int32) (4-_uint32 > 10-_uint8);
  (240.c:127#2423)^0- =(int32) (4-_uint32 > coerce[0,4294967295] 9-_int16);
  (240.c:128#2436)^0- =(int32) (4-_uint32 > 8-_uint16);
  (240.c:129#2449)^0- =(int32) (4-_uint32 > coerce[0,4294967295] 7-_int32);
  (240.c:130#2462)^0- =(int32) (4-_uint32 > 6-_uint32);
  (240.c:131#2475)^0- =(int32) (4-_uint32 > coerce[0,4294967295] 5-_int32);
  (240.c:132#2488)^0- =(int32) (4-_uint32 > 4-_uint32);
  (240.c:133#2501)^0- =(int32) (4-_uint32 > (uint32) 3-_ptr);
  (240.c:134#2514)^0- =(int32) ((float32 <= uint32) 4-_uint32 > 2-_float32);
  (240.c:135#2527)^0- =(int32) ((float64 <= uint32) 4-_uint32 > 1-_float64);
  (240.c:141#2753)^0- =(int32) ((uint32) 3-_ptr > coerce[0,4294967295] 7-_int32);
  (240.c:142#2766)^0- =(int32) ((uint32) 3-_ptr > 6-_uint32);
  (240.c:143#2779)^0- =(int32) ((uint32) 3-_ptr > coerce[0,4294967295] 5-_int32);
  (240.c:144#2792)^0- =(int32) ((uint32) 3-_ptr > 4-_uint32);
  (240.c:145#2805)^0- =(int32) ((uint32) 3-_ptr > (uint32) 3-_ptr);
  (240.c:149#2895)^0- =(int32) (2-_float32 > (float32 <= int8) 11-_int8);
  (240.c:150#2908)^0- =(int32) (2-_float32 > (float32 <= uint8) 10-_uint8);
  (240.c:151#2921)^0- =(int32) (2-_float32 > (float32 <= int16) 9-_int16);
  (240.c:152#2934)^0- =(int32) (2-_float32 > (float32 <= uint16) 8-_uint16);
  (240.c:153#2947)^0- =(int32) (2-_float32 > (float32 <= int32) 7-_int32);
  (240.c:154#2960)^0- =(int32) (2-_float32 > (float32 <= uint32) 6-_uint32);
  (240.c:155#2973)^0- =(int32) (2-_float32 > (float32 <= int32) 5-_int32);
  (240.c:156#2986)^0- =(int32) (2-_float32 > (float32 <= uint32) 4-_uint32);
  (240.c:158#3037)^0- =(int32) (2-_float32 > 2-_float32);
  (240.c:159#3050)^0- =(int32) ((float64 <= float32) 2-_float32 > 1-_float64);
  (240.c:161#3064)^0- =(int32) (1-_float64 > (float64 <= int8) 11-_int8);
  (240.c:162#3077)^0- =(int32) (1-_float64 > (float64 <= uint8) 10-_uint8);
  (240.c:163#3090)^0- =(int32) (1-_float64 > (float64 <= int16) 9-_int16);
  (240.c:164#3103)^0- =(int32) (1-_float64 > (float64 <= uint16) 8-_uint16);
  (240.c:165#3116)^0- =(int32) (1-_float64 > (float64 <= int32) 7-_int32);
  (240.c:166#3129)^0- =(int32) (1-_float64 > (float64 <= uint32) 6-_uint32);
  (240.c:167#3142)^0- =(int32) (1-_float64 > (float64 <= int32) 5-_int32);
  (240.c:168#3155)^0- =(int32) (1-_float64 > (float64 <= uint32) 4-_uint32);
  (240.c:170#3206)^0- =(int32) (1-_float64 > (float64 <= float32) 2-_float32);
  (240.c:171#3219)^0- =(int32) (1-_float64 > 1-_float64);
}


