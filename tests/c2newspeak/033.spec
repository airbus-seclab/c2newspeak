Newspeak output
---------------
033.c
main() {
  (033.c:30#1133)^int8 a;
  (033.c:31#1152)^uint8 b;
  (033.c:32#1163)^int16 c;
  (033.c:33#1183)^uint16 d;
  (033.c:34#1192)^int32 e;
  (033.c:35#1210)^uint32 f;
  (033.c:36#1221)^ptr ptr;
  (033.c:38#1231)^6- =(int8) coerce[-128,127] (6-_int8 + 5-_uint8);
  (033.c:39#1244)^6- =(int8) coerce[-128,127] (6-_int8 + 4-_int16);
  (033.c:40#1257)^6- =(int8) coerce[-128,127] (6-_int8 + 3-_uint16);
  (033.c:41#1270)^6- =(int8) coerce[-128,127] (6-_int8 + 2-_int32);
  (033.c:42#1283)^6- =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] 6-_int8 + 1-_uint32);
  (033.c:43#1296)^6- =(int8) coerce[-128,127] (5-_uint8 + 4-_int16);
  (033.c:44#1309)^6- =(int8) coerce[-128,127] (5-_uint8 + 3-_uint16);
  (033.c:45#1322)^6- =(int8) coerce[-128,127] (5-_uint8 + 2-_int32);
  (033.c:46#1335)^6- =(int8) coerce[-128,127] coerce[0,4294967295] (5-_uint8 + 1-_uint32);
  (033.c:47#1348)^6- =(int8) coerce[-128,127] (4-_int16 + 3-_uint16);
  (033.c:48#1361)^6- =(int8) coerce[-128,127] (4-_int16 + 2-_int32);
  (033.c:49#1374)^6- =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] 4-_int16 + 1-_uint32);
  (033.c:50#1387)^6- =(int8) coerce[-128,127] (3-_uint16 + 2-_int32);
  (033.c:51#1400)^6- =(int8) coerce[-128,127] coerce[0,4294967295] (3-_uint16 + 1-_uint32);
  (033.c:52#1413)^6- =(int8) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] 2-_int32 + 1-_uint32);
  (033.c:54#1427)^5- =(uint8) coerce[0,255] (6-_int8 + 5-_uint8);
  (033.c:55#1440)^5- =(uint8) coerce[0,255] (6-_int8 + 4-_int16);
  (033.c:56#1453)^5- =(uint8) coerce[0,255] (6-_int8 + 3-_uint16);
  (033.c:57#1466)^5- =(uint8) coerce[0,255] (6-_int8 + 2-_int32);
  (033.c:58#1479)^5- =(uint8) coerce[0,255] (coerce[0,4294967295] 6-_int8 + 1-_uint32);
  (033.c:59#1492)^5- =(uint8) coerce[0,255] (5-_uint8 + 4-_int16);
  (033.c:60#1505)^5- =(uint8) coerce[0,255] (5-_uint8 + 3-_uint16);
  (033.c:61#1518)^5- =(uint8) coerce[0,255] (5-_uint8 + 2-_int32);
  (033.c:62#1531)^5- =(uint8) coerce[0,255] (5-_uint8 + 1-_uint32);
  (033.c:63#1544)^5- =(uint8) coerce[0,255] (4-_int16 + 3-_uint16);
  (033.c:64#1557)^5- =(uint8) coerce[0,255] (4-_int16 + 2-_int32);
  (033.c:65#1570)^5- =(uint8) coerce[0,255] (coerce[0,4294967295] 4-_int16 + 1-_uint32);
  (033.c:66#1583)^5- =(uint8) coerce[0,255] (3-_uint16 + 2-_int32);
  (033.c:67#1596)^5- =(uint8) coerce[0,255] (3-_uint16 + 1-_uint32);
  (033.c:68#1609)^5- =(uint8) coerce[0,255] (coerce[0,4294967295] 2-_int32 + 1-_uint32);
  (033.c:70#1623)^4- =(int16) coerce[-32768,32767] (6-_int8 + 5-_uint8);
  (033.c:71#1636)^4- =(int16) coerce[-32768,32767] (6-_int8 + 4-_int16);
  (033.c:72#1649)^4- =(int16) coerce[-32768,32767] (6-_int8 + 3-_uint16);
  (033.c:73#1662)^4- =(int16) coerce[-32768,32767] (6-_int8 + 2-_int32);
  (033.c:74#1675)^4- =(int16) coerce[-32768,32767] coerce[0,4294967295] (coerce[0,4294967295] 6-_int8 + 1-_uint32);
  (033.c:75#1688)^4- =(int16) coerce[-32768,32767] (5-_uint8 + 4-_int16);
  (033.c:76#1701)^4- =(int16) coerce[-32768,32767] (5-_uint8 + 3-_uint16);
  (033.c:77#1714)^4- =(int16) coerce[-32768,32767] (5-_uint8 + 2-_int32);
  (033.c:78#1727)^4- =(int16) coerce[-32768,32767] coerce[0,4294967295] (5-_uint8 + 1-_uint32);
  (033.c:79#1740)^4- =(int16) coerce[-32768,32767] (4-_int16 + 3-_uint16);
  (033.c:80#1753)^4- =(int16) coerce[-32768,32767] (4-_int16 + 2-_int32);
  (033.c:81#1766)^4- =(int16) coerce[-32768,32767] coerce[0,4294967295] (coerce[0,4294967295] 4-_int16 + 1-_uint32);
  (033.c:82#1779)^4- =(int16) coerce[-32768,32767] (3-_uint16 + 2-_int32);
  (033.c:83#1792)^4- =(int16) coerce[-32768,32767] coerce[0,4294967295] (3-_uint16 + 1-_uint32);
  (033.c:84#1805)^4- =(int16) coerce[-32768,32767] coerce[0,4294967295] (coerce[0,4294967295] 2-_int32 + 1-_uint32);
  (033.c:86#1819)^3- =(uint16) coerce[0,65535] (6-_int8 + 5-_uint8);
  (033.c:87#1832)^3- =(uint16) coerce[0,65535] (6-_int8 + 4-_int16);
  (033.c:88#1845)^3- =(uint16) coerce[0,65535] (6-_int8 + 3-_uint16);
  (033.c:89#1858)^3- =(uint16) coerce[0,65535] (6-_int8 + 2-_int32);
  (033.c:90#1871)^3- =(uint16) coerce[0,65535] (coerce[0,4294967295] 6-_int8 + 1-_uint32);
  (033.c:91#1884)^3- =(uint16) coerce[0,65535] (5-_uint8 + 4-_int16);
  (033.c:92#1897)^3- =(uint16) coerce[0,65535] (5-_uint8 + 3-_uint16);
  (033.c:93#1910)^3- =(uint16) coerce[0,65535] (5-_uint8 + 2-_int32);
  (033.c:94#1923)^3- =(uint16) coerce[0,65535] (5-_uint8 + 1-_uint32);
  (033.c:95#1936)^3- =(uint16) coerce[0,65535] (4-_int16 + 3-_uint16);
  (033.c:96#1949)^3- =(uint16) coerce[0,65535] (4-_int16 + 2-_int32);
  (033.c:97#1962)^3- =(uint16) coerce[0,65535] (coerce[0,4294967295] 4-_int16 + 1-_uint32);
  (033.c:98#1975)^3- =(uint16) coerce[0,65535] (3-_uint16 + 2-_int32);
  (033.c:99#1988)^3- =(uint16) coerce[0,65535] (3-_uint16 + 1-_uint32);
  (033.c:100#2001)^3- =(uint16) coerce[0,65535] (coerce[0,4294967295] 2-_int32 + 1-_uint32);
  (033.c:102#2015)^2- =(int32) coerce[-2147483648,2147483647] (6-_int8 + 5-_uint8);
  (033.c:103#2028)^2- =(int32) coerce[-2147483648,2147483647] (6-_int8 + 4-_int16);
  (033.c:104#2041)^2- =(int32) coerce[-2147483648,2147483647] (6-_int8 + 3-_uint16);
  (033.c:105#2054)^2- =(int32) coerce[-2147483648,2147483647] (6-_int8 + 2-_int32);
  (033.c:106#2067)^2- =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (coerce[0,4294967295] 6-_int8 + 1-_uint32);
  (033.c:107#2080)^2- =(int32) coerce[-2147483648,2147483647] (5-_uint8 + 4-_int16);
  (033.c:108#2093)^2- =(int32) coerce[-2147483648,2147483647] (5-_uint8 + 3-_uint16);
  (033.c:109#2106)^2- =(int32) coerce[-2147483648,2147483647] (5-_uint8 + 2-_int32);
  (033.c:110#2119)^2- =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (5-_uint8 + 1-_uint32);
  (033.c:111#2132)^2- =(int32) coerce[-2147483648,2147483647] (4-_int16 + 3-_uint16);
  (033.c:112#2145)^2- =(int32) coerce[-2147483648,2147483647] (4-_int16 + 2-_int32);
  (033.c:113#2158)^2- =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (coerce[0,4294967295] 4-_int16 + 1-_uint32);
  (033.c:114#2171)^2- =(int32) coerce[-2147483648,2147483647] (3-_uint16 + 2-_int32);
  (033.c:115#2184)^2- =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (3-_uint16 + 1-_uint32);
  (033.c:116#2197)^2- =(int32) coerce[-2147483648,2147483647] coerce[0,4294967295] (coerce[0,4294967295] 2-_int32 + 1-_uint32);
  (033.c:118#2211)^1- =(uint32) coerce[0,4294967295] coerce[-2147483648,2147483647] (6-_int8 + 5-_uint8);
  (033.c:119#2224)^1- =(uint32) coerce[0,4294967295] coerce[-2147483648,2147483647] (6-_int8 + 4-_int16);
  (033.c:120#2237)^1- =(uint32) coerce[0,4294967295] coerce[-2147483648,2147483647] (6-_int8 + 3-_uint16);
  (033.c:121#2250)^1- =(uint32) coerce[0,4294967295] coerce[-2147483648,2147483647] (6-_int8 + 2-_int32);
  (033.c:122#2263)^1- =(uint32) coerce[0,4294967295] (coerce[0,4294967295] 6-_int8 + 1-_uint32);
  (033.c:123#2276)^1- =(uint32) coerce[0,4294967295] coerce[-2147483648,2147483647] (5-_uint8 + 4-_int16);
  (033.c:124#2289)^1- =(uint32) coerce[0,4294967295] coerce[-2147483648,2147483647] (5-_uint8 + 3-_uint16);
  (033.c:125#2302)^1- =(uint32) coerce[0,4294967295] coerce[-2147483648,2147483647] (5-_uint8 + 2-_int32);
  (033.c:126#2315)^1- =(uint32) coerce[0,4294967295] (5-_uint8 + 1-_uint32);
  (033.c:127#2328)^1- =(uint32) coerce[0,4294967295] coerce[-2147483648,2147483647] (4-_int16 + 3-_uint16);
  (033.c:128#2341)^1- =(uint32) coerce[0,4294967295] coerce[-2147483648,2147483647] (4-_int16 + 2-_int32);
  (033.c:129#2354)^1- =(uint32) coerce[0,4294967295] (coerce[0,4294967295] 4-_int16 + 1-_uint32);
  (033.c:130#2367)^1- =(uint32) coerce[0,4294967295] coerce[-2147483648,2147483647] (3-_uint16 + 2-_int32);
  (033.c:131#2380)^1- =(uint32) coerce[0,4294967295] (3-_uint16 + 1-_uint32);
  (033.c:132#2393)^1- =(uint32) coerce[0,4294967295] (coerce[0,4294967295] 2-_int32 + 1-_uint32);
  (033.c:134#2407)^0- =(ptr) (0-_ptr + (6-_int8 * 8));
  (033.c:135#2424)^0- =(ptr) (0-_ptr + (5-_uint8 * 8));
  (033.c:136#2441)^0- =(ptr) (0-_ptr + (4-_int16 * 8));
  (033.c:137#2458)^0- =(ptr) (0-_ptr + (3-_uint16 * 8));
  (033.c:138#2475)^0- =(ptr) (0-_ptr + (2-_int32 * 8));
  (033.c:139#2492)^0- =(ptr) (0-_ptr + (1-_uint32 * 8));
}

