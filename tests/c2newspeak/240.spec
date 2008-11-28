Warning: 240.c:97: dirty cast from integer to pointer accepted
Warning: 240.c:109: dirty cast from integer to pointer accepted
Warning: 240.c:121: dirty cast from integer to pointer accepted
Warning: 240.c:133: dirty cast from integer to pointer accepted
Warning: 240.c:141: dirty cast from integer to pointer accepted
Warning: 240.c:142: dirty cast from integer to pointer accepted
Warning: 240.c:143: dirty cast from integer to pointer accepted
Warning: 240.c:144: dirty cast from integer to pointer accepted
Newspeak output
---------------
240.c
main() {
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
  (240.c:41#2)^0- =(int32) (11-_int8 > 11-_int8);
  (240.c:42#2)^0- =(int32) (11-_int8 > 10-_uint8);
  (240.c:43#2)^0- =(int32) (11-_int8 > 9-_int16);
  (240.c:44#2)^0- =(int32) (11-_int8 > 8-_uint16);
  (240.c:45#2)^0- =(int32) (11-_int8 > 7-_int32);
  (240.c:46#2)^0- =(int32) (coerce[0,4294967295] 11-_int8 > 6-_uint32);
  (240.c:47#2)^0- =(int32) (11-_int8 > 5-_int32);
  (240.c:48#2)^0- =(int32) (coerce[0,4294967295] 11-_int8 > 4-_uint32);
  (240.c:50#2)^0- =(int32) ((float32 <= int8) 11-_int8 > 2-_float32);
  (240.c:51#2)^0- =(int32) ((float64 <= int8) 11-_int8 > 1-_float64);
  (240.c:53#2)^0- =(int32) (10-_uint8 > 11-_int8);
  (240.c:54#2)^0- =(int32) (10-_uint8 > 10-_uint8);
  (240.c:55#2)^0- =(int32) (10-_uint8 > 9-_int16);
  (240.c:56#2)^0- =(int32) (10-_uint8 > 8-_uint16);
  (240.c:57#2)^0- =(int32) (10-_uint8 > 7-_int32);
  (240.c:58#2)^0- =(int32) (10-_uint8 > 6-_uint32);
  (240.c:59#2)^0- =(int32) (10-_uint8 > 5-_int32);
  (240.c:60#2)^0- =(int32) (10-_uint8 > 4-_uint32);
  (240.c:62#2)^0- =(int32) ((float32 <= uint8) 10-_uint8 > 2-_float32);
  (240.c:63#2)^0- =(int32) ((float64 <= uint8) 10-_uint8 > 1-_float64);
  (240.c:65#2)^0- =(int32) (9-_int16 > 11-_int8);
  (240.c:66#2)^0- =(int32) (9-_int16 > 10-_uint8);
  (240.c:67#2)^0- =(int32) (9-_int16 > 9-_int16);
  (240.c:68#2)^0- =(int32) (9-_int16 > 8-_uint16);
  (240.c:69#2)^0- =(int32) (9-_int16 > 7-_int32);
  (240.c:70#2)^0- =(int32) (coerce[0,4294967295] 9-_int16 > 6-_uint32);
  (240.c:71#2)^0- =(int32) (9-_int16 > 5-_int32);
  (240.c:72#2)^0- =(int32) (coerce[0,4294967295] 9-_int16 > 4-_uint32);
  (240.c:74#2)^0- =(int32) ((float32 <= int16) 9-_int16 > 2-_float32);
  (240.c:75#2)^0- =(int32) ((float64 <= int16) 9-_int16 > 1-_float64);
  (240.c:77#2)^0- =(int32) (8-_uint16 > 11-_int8);
  (240.c:78#2)^0- =(int32) (8-_uint16 > 10-_uint8);
  (240.c:79#2)^0- =(int32) (8-_uint16 > 9-_int16);
  (240.c:80#2)^0- =(int32) (8-_uint16 > 8-_uint16);
  (240.c:81#2)^0- =(int32) (8-_uint16 > 7-_int32);
  (240.c:82#2)^0- =(int32) (8-_uint16 > 6-_uint32);
  (240.c:83#2)^0- =(int32) (8-_uint16 > 5-_int32);
  (240.c:84#2)^0- =(int32) (8-_uint16 > 4-_uint32);
  (240.c:86#2)^0- =(int32) ((float32 <= uint16) 8-_uint16 > 2-_float32);
  (240.c:87#2)^0- =(int32) ((float64 <= uint16) 8-_uint16 > 1-_float64);
  (240.c:89#2)^0- =(int32) (7-_int32 > 11-_int8);
  (240.c:90#2)^0- =(int32) (7-_int32 > 10-_uint8);
  (240.c:91#2)^0- =(int32) (7-_int32 > 9-_int16);
  (240.c:92#2)^0- =(int32) (7-_int32 > 8-_uint16);
  (240.c:93#2)^0- =(int32) (7-_int32 > 7-_int32);
  (240.c:94#2)^0- =(int32) (coerce[0,4294967295] 7-_int32 > 6-_uint32);
  (240.c:95#2)^0- =(int32) (7-_int32 > 5-_int32);
  (240.c:96#2)^0- =(int32) (coerce[0,4294967295] 7-_int32 > 4-_uint32);
  (240.c:97#2)^0- =(int32) ((ptr) 7-_int32 > 3-_ptr);
  (240.c:98#2)^0- =(int32) ((float32 <= int32) 7-_int32 > 2-_float32);
  (240.c:99#2)^0- =(int32) ((float64 <= int32) 7-_int32 > 1-_float64);
  (240.c:101#2)^0- =(int32) (6-_uint32 > coerce[0,4294967295] 11-_int8);
  (240.c:102#2)^0- =(int32) (6-_uint32 > 10-_uint8);
  (240.c:103#2)^0- =(int32) (6-_uint32 > coerce[0,4294967295] 9-_int16);
  (240.c:104#2)^0- =(int32) (6-_uint32 > 8-_uint16);
  (240.c:105#2)^0- =(int32) (6-_uint32 > coerce[0,4294967295] 7-_int32);
  (240.c:106#2)^0- =(int32) (6-_uint32 > 6-_uint32);
  (240.c:107#2)^0- =(int32) (6-_uint32 > coerce[0,4294967295] 5-_int32);
  (240.c:108#2)^0- =(int32) (6-_uint32 > 4-_uint32);
  (240.c:109#2)^0- =(int32) ((ptr) 6-_uint32 > 3-_ptr);
  (240.c:110#2)^0- =(int32) ((float32 <= uint32) 6-_uint32 > 2-_float32);
  (240.c:111#2)^0- =(int32) ((float64 <= uint32) 6-_uint32 > 1-_float64);
  (240.c:113#2)^0- =(int32) (5-_int32 > 11-_int8);
  (240.c:114#2)^0- =(int32) (5-_int32 > 10-_uint8);
  (240.c:115#2)^0- =(int32) (5-_int32 > 9-_int16);
  (240.c:116#2)^0- =(int32) (5-_int32 > 8-_uint16);
  (240.c:117#2)^0- =(int32) (5-_int32 > 7-_int32);
  (240.c:118#2)^0- =(int32) (coerce[0,4294967295] 5-_int32 > 6-_uint32);
  (240.c:119#2)^0- =(int32) (5-_int32 > 5-_int32);
  (240.c:120#2)^0- =(int32) (coerce[0,4294967295] 5-_int32 > 4-_uint32);
  (240.c:121#2)^0- =(int32) ((ptr) 5-_int32 > 3-_ptr);
  (240.c:122#2)^0- =(int32) ((float32 <= int32) 5-_int32 > 2-_float32);
  (240.c:123#2)^0- =(int32) ((float64 <= int32) 5-_int32 > 1-_float64);
  (240.c:125#2)^0- =(int32) (4-_uint32 > coerce[0,4294967295] 11-_int8);
  (240.c:126#2)^0- =(int32) (4-_uint32 > 10-_uint8);
  (240.c:127#2)^0- =(int32) (4-_uint32 > coerce[0,4294967295] 9-_int16);
  (240.c:128#2)^0- =(int32) (4-_uint32 > 8-_uint16);
  (240.c:129#2)^0- =(int32) (4-_uint32 > coerce[0,4294967295] 7-_int32);
  (240.c:130#2)^0- =(int32) (4-_uint32 > 6-_uint32);
  (240.c:131#2)^0- =(int32) (4-_uint32 > coerce[0,4294967295] 5-_int32);
  (240.c:132#2)^0- =(int32) (4-_uint32 > 4-_uint32);
  (240.c:133#2)^0- =(int32) ((ptr) 4-_uint32 > 3-_ptr);
  (240.c:134#2)^0- =(int32) ((float32 <= uint32) 4-_uint32 > 2-_float32);
  (240.c:135#2)^0- =(int32) ((float64 <= uint32) 4-_uint32 > 1-_float64);
  (240.c:141#2)^0- =(int32) (3-_ptr > (ptr) 7-_int32);
  (240.c:142#2)^0- =(int32) (3-_ptr > (ptr) 6-_uint32);
  (240.c:143#2)^0- =(int32) (3-_ptr > (ptr) 5-_int32);
  (240.c:144#2)^0- =(int32) (3-_ptr > (ptr) 4-_uint32);
  (240.c:145#2)^0- =(int32) (3-_ptr > 3-_ptr);
  (240.c:149#2)^0- =(int32) (2-_float32 > (float32 <= int8) 11-_int8);
  (240.c:150#2)^0- =(int32) (2-_float32 > (float32 <= uint8) 10-_uint8);
  (240.c:151#2)^0- =(int32) (2-_float32 > (float32 <= int16) 9-_int16);
  (240.c:152#2)^0- =(int32) (2-_float32 > (float32 <= uint16) 8-_uint16);
  (240.c:153#2)^0- =(int32) (2-_float32 > (float32 <= int32) 7-_int32);
  (240.c:154#2)^0- =(int32) (2-_float32 > (float32 <= uint32) 6-_uint32);
  (240.c:155#2)^0- =(int32) (2-_float32 > (float32 <= int32) 5-_int32);
  (240.c:156#2)^0- =(int32) (2-_float32 > (float32 <= uint32) 4-_uint32);
  (240.c:158#2)^0- =(int32) (2-_float32 > 2-_float32);
  (240.c:159#2)^0- =(int32) ((float64 <= float32) 2-_float32 > 1-_float64);
  (240.c:161#2)^0- =(int32) (1-_float64 > (float64 <= int8) 11-_int8);
  (240.c:162#2)^0- =(int32) (1-_float64 > (float64 <= uint8) 10-_uint8);
  (240.c:163#2)^0- =(int32) (1-_float64 > (float64 <= int16) 9-_int16);
  (240.c:164#2)^0- =(int32) (1-_float64 > (float64 <= uint16) 8-_uint16);
  (240.c:165#2)^0- =(int32) (1-_float64 > (float64 <= int32) 7-_int32);
  (240.c:166#2)^0- =(int32) (1-_float64 > (float64 <= uint32) 6-_uint32);
  (240.c:167#2)^0- =(int32) (1-_float64 > (float64 <= int32) 5-_int32);
  (240.c:168#2)^0- =(int32) (1-_float64 > (float64 <= uint32) 4-_uint32);
  (240.c:170#2)^0- =(int32) (1-_float64 > (float64 <= float32) 2-_float32);
  (240.c:171#2)^0- =(int32) (1-_float64 > 1-_float64);
}


