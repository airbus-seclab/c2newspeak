void (001.c:29#5)^main(void) {
  (001.c:30#7)^int1 a;
  (001.c:31#16)^uint1 b;
  (001.c:32#8)^int2 c;
  (001.c:33#17)^uint2 d;
  (001.c:34#6)^int4 e;
  (001.c:35#15)^uint4 f;
  (001.c:36#8)^ptr ptr;
  (001.c:38#2)^a =(int1) coerce[-128,127] (a_int1 + b_uint1);
  (001.c:39#2)^a =(int1) coerce[-128,127] (a_int1 + c_int2);
  (001.c:40#2)^a =(int1) coerce[-128,127] (a_int1 + d_uint2);
  (001.c:41#2)^a =(int1) coerce[-128,127] (a_int1 + e_int4);
  (001.c:42#2)^a =(int1) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] a_int1 + f_uint4);
  (001.c:43#2)^a =(int1) coerce[-128,127] (b_uint1 + c_int2);
  (001.c:44#2)^a =(int1) coerce[-128,127] (b_uint1 + d_uint2);
  (001.c:45#2)^a =(int1) coerce[-128,127] (b_uint1 + e_int4);
  (001.c:46#2)^a =(int1) coerce[-128,127] coerce[0,4294967295] (b_uint1 + f_uint4);
  (001.c:47#2)^a =(int1) coerce[-128,127] (c_int2 + d_uint2);
  (001.c:48#2)^a =(int1) coerce[-128,127] (c_int2 + e_int4);
  (001.c:49#2)^a =(int1) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] c_int2 + f_uint4);
  (001.c:50#2)^a =(int1) coerce[-128,127] (d_uint2 + e_int4);
  (001.c:51#2)^a =(int1) coerce[-128,127] coerce[0,4294967295] (d_uint2 + f_uint4);
  (001.c:52#2)^a =(int1) coerce[-128,127] coerce[0,4294967295] (coerce[0,4294967295] e_int4 + f_uint4);
  (001.c:54#2)^b =(uint1) coerce[0,255] (a_int1 + b_uint1);
  (001.c:55#2)^b =(uint1) coerce[0,255] (a_int1 + c_int2);
  (001.c:56#2)^b =(uint1) coerce[0,255] (a_int1 + d_uint2);
  (001.c:57#2)^b =(uint1) coerce[0,255] (a_int1 + e_int4);
  (001.c:58#2)^b =(uint1) coerce[0,255] (coerce[0,4294967295] a_int1 + f_uint4);
  (001.c:59#2)^b =(uint1) coerce[0,255] (b_uint1 + c_int2);
  (001.c:60#2)^b =(uint1) coerce[0,255] (b_uint1 + d_uint2);
  (001.c:61#2)^b =(uint1) coerce[0,255] (b_uint1 + e_int4);
  (001.c:62#2)^b =(uint1) coerce[0,255] (b_uint1 + f_uint4);
  (001.c:63#2)^b =(uint1) coerce[0,255] (c_int2 + d_uint2);
  (001.c:64#2)^b =(uint1) coerce[0,255] (c_int2 + e_int4);
  (001.c:65#2)^b =(uint1) coerce[0,255] (coerce[0,4294967295] c_int2 + f_uint4);
  (001.c:66#2)^b =(uint1) coerce[0,255] (d_uint2 + e_int4);
  (001.c:67#2)^b =(uint1) coerce[0,255] (d_uint2 + f_uint4);
  (001.c:68#2)^b =(uint1) coerce[0,255] (coerce[0,4294967295] e_int4 + f_uint4);
  (001.c:70#2)^c =(int2) coerce[-32768,32767] (a_int1 + b_uint1);
  (001.c:71#2)^c =(int2) coerce[-32768,32767] (a_int1 + c_int2);
  (001.c:72#2)^c =(int2) coerce[-32768,32767] (a_int1 + d_uint2);
  (001.c:73#2)^c =(int2) coerce[-32768,32767] (a_int1 + e_int4);
  (001.c:74#2)^c =(int2) coerce[-32768,32767] coerce[0,4294967295] (coerce[0,4294967295] a_int1 + f_uint4);
  (001.c:75#2)^c =(int2) coerce[-32768,32767] (b_uint1 + c_int2);
  (001.c:76#2)^c =(int2) coerce[-32768,32767] (b_uint1 + d_uint2);
  (001.c:77#2)^c =(int2) coerce[-32768,32767] (b_uint1 + e_int4);
  (001.c:78#2)^c =(int2) coerce[-32768,32767] coerce[0,4294967295] (b_uint1 + f_uint4);
  (001.c:79#2)^c =(int2) coerce[-32768,32767] (c_int2 + d_uint2);
  (001.c:80#2)^c =(int2) coerce[-32768,32767] (c_int2 + e_int4);
  (001.c:81#2)^c =(int2) coerce[-32768,32767] coerce[0,4294967295] (coerce[0,4294967295] c_int2 + f_uint4);
  (001.c:82#2)^c =(int2) coerce[-32768,32767] (d_uint2 + e_int4);
  (001.c:83#2)^c =(int2) coerce[-32768,32767] coerce[0,4294967295] (d_uint2 + f_uint4);
  (001.c:84#2)^c =(int2) coerce[-32768,32767] coerce[0,4294967295] (coerce[0,4294967295] e_int4 + f_uint4);
  (001.c:86#2)^d =(uint2) coerce[0,65535] (a_int1 + b_uint1);
  (001.c:87#2)^d =(uint2) coerce[0,65535] (a_int1 + c_int2);
  (001.c:88#2)^d =(uint2) coerce[0,65535] (a_int1 + d_uint2);
  (001.c:89#2)^d =(uint2) coerce[0,65535] (a_int1 + e_int4);
  (001.c:90#2)^d =(uint2) coerce[0,65535] (coerce[0,4294967295] a_int1 + f_uint4);
  (001.c:91#2)^d =(uint2) coerce[0,65535] (b_uint1 + c_int2);
  (001.c:92#2)^d =(uint2) coerce[0,65535] (b_uint1 + d_uint2);
  (001.c:93#2)^d =(uint2) coerce[0,65535] (b_uint1 + e_int4);
  (001.c:94#2)^d =(uint2) coerce[0,65535] (b_uint1 + f_uint4);
  (001.c:95#2)^d =(uint2) coerce[0,65535] (c_int2 + d_uint2);
  (001.c:96#2)^d =(uint2) coerce[0,65535] (c_int2 + e_int4);
  (001.c:97#2)^d =(uint2) coerce[0,65535] (coerce[0,4294967295] c_int2 + f_uint4);
  (001.c:98#2)^d =(uint2) coerce[0,65535] (d_uint2 + e_int4);
  (001.c:99#2)^d =(uint2) coerce[0,65535] (d_uint2 + f_uint4);
  (001.c:100#2)^d =(uint2) coerce[0,65535] (coerce[0,4294967295] e_int4 + f_uint4);
  (001.c:102#2)^e =(int4) coerce[-2147483648,2147483647] (a_int1 + b_uint1);
  (001.c:103#2)^e =(int4) coerce[-2147483648,2147483647] (a_int1 + c_int2);
  (001.c:104#2)^e =(int4) coerce[-2147483648,2147483647] (a_int1 + d_uint2);
  (001.c:105#2)^e =(int4) coerce[-2147483648,2147483647] (a_int1 + e_int4);
  (001.c:106#2)^e =(int4) coerce[-2147483648,2147483647] coerce[0,4294967295] (coerce[0,4294967295] a_int1 + f_uint4);
  (001.c:107#2)^e =(int4) coerce[-2147483648,2147483647] (b_uint1 + c_int2);
  (001.c:108#2)^e =(int4) coerce[-2147483648,2147483647] (b_uint1 + d_uint2);
  (001.c:109#2)^e =(int4) coerce[-2147483648,2147483647] (b_uint1 + e_int4);
  (001.c:110#2)^e =(int4) coerce[-2147483648,2147483647] coerce[0,4294967295] (b_uint1 + f_uint4);
  (001.c:111#2)^e =(int4) coerce[-2147483648,2147483647] (c_int2 + d_uint2);
  (001.c:112#2)^e =(int4) coerce[-2147483648,2147483647] (c_int2 + e_int4);
  (001.c:113#2)^e =(int4) coerce[-2147483648,2147483647] coerce[0,4294967295] (coerce[0,4294967295] c_int2 + f_uint4);
  (001.c:114#2)^e =(int4) coerce[-2147483648,2147483647] (d_uint2 + e_int4);
  (001.c:115#2)^e =(int4) coerce[-2147483648,2147483647] coerce[0,4294967295] (d_uint2 + f_uint4);
  (001.c:116#2)^e =(int4) coerce[-2147483648,2147483647] coerce[0,4294967295] (coerce[0,4294967295] e_int4 + f_uint4);
  (001.c:118#2)^f =(uint4) coerce[0,4294967295] coerce[-2147483648,2147483647] (a_int1 + b_uint1);
  (001.c:119#2)^f =(uint4) coerce[0,4294967295] coerce[-2147483648,2147483647] (a_int1 + c_int2);
  (001.c:120#2)^f =(uint4) coerce[0,4294967295] coerce[-2147483648,2147483647] (a_int1 + d_uint2);
  (001.c:121#2)^f =(uint4) coerce[0,4294967295] coerce[-2147483648,2147483647] (a_int1 + e_int4);
  (001.c:122#2)^f =(uint4) coerce[0,4294967295] (coerce[0,4294967295] a_int1 + f_uint4);
  (001.c:123#2)^f =(uint4) coerce[0,4294967295] coerce[-2147483648,2147483647] (b_uint1 + c_int2);
  (001.c:124#2)^f =(uint4) coerce[0,4294967295] coerce[-2147483648,2147483647] (b_uint1 + d_uint2);
  (001.c:125#2)^f =(uint4) coerce[0,4294967295] coerce[-2147483648,2147483647] (b_uint1 + e_int4);
  (001.c:126#2)^f =(uint4) coerce[0,4294967295] (b_uint1 + f_uint4);
  (001.c:127#2)^f =(uint4) coerce[0,4294967295] coerce[-2147483648,2147483647] (c_int2 + d_uint2);
  (001.c:128#2)^f =(uint4) coerce[0,4294967295] coerce[-2147483648,2147483647] (c_int2 + e_int4);
  (001.c:129#2)^f =(uint4) coerce[0,4294967295] (coerce[0,4294967295] c_int2 + f_uint4);
  (001.c:130#2)^f =(uint4) coerce[0,4294967295] coerce[-2147483648,2147483647] (d_uint2 + e_int4);
  (001.c:131#2)^f =(uint4) coerce[0,4294967295] (d_uint2 + f_uint4);
  (001.c:132#2)^f =(uint4) coerce[0,4294967295] (coerce[0,4294967295] e_int4 + f_uint4);
  (001.c:134#2)^ptr =(ptr) (ptr_ptr + a_int1);
  (001.c:135#2)^ptr =(ptr) (ptr_ptr + b_uint1);
  (001.c:136#2)^ptr =(ptr) (ptr_ptr + c_int2);
  (001.c:137#2)^ptr =(ptr) (ptr_ptr + d_uint2);
  (001.c:138#2)^ptr =(ptr) (ptr_ptr + e_int4);
  (001.c:139#2)^ptr =(ptr) (ptr_ptr + f_uint4);
}

