Newspeak output
---------------
t234.adb
void t234(void) {
  (t234.adb:7#13)^int32 i;
  (t234.adb:7#13)^int32 j;
  (t234.adb:7#13)^int32 k;
  (t234.adb:8#13)^uint1 r;
  (t234.adb:12#8)^i =(int32) 0;
  (t234.adb:13#8)^i =(int32) 500;
  (t234.adb:14#8)^i =(int32) -500;
  (t234.adb:19#8)^r =(uint1) (i_int32 ==_int32 j_int32);
  (t234.adb:22#8)^r =(uint1) ! (i_int32 ==_int32 j_int32);
  (t234.adb:25#8)^r =(uint1) (j_int32 > i_int32);
  (t234.adb:28#8)^r =(uint1) ! (i_int32 > j_int32);
  (t234.adb:31#8)^r =(uint1) (i_int32 > j_int32);
  (t234.adb:34#8)^r =(uint1) ! (j_int32 > i_int32);
  (t234.adb:37#8)^k =(int32) belongs[-2147483648,2147483647] (i_int32 + j_int32);
  (t234.adb:40#8)^k =(int32) belongs[-2147483648,2147483647] (i_int32 - j_int32);
  (t234.adb:43#8)^k =(int32) j_int32;
  (t234.adb:46#8)^k =(int32) belongs[-2147483648,2147483647] (0 - j_int32);
  (t234.adb:55#8)^k =(int32) belongs[-2147483648,2147483647] (i_int32 * j_int32);
  (t234.adb:58#8)^k =(int32) belongs[-2147483648,2147483647] (i_int32 / j_int32);
  (t234.adb:64#8)^k =(int32) belongs[-2147483648,2147483647] (i_int32 % j_int32);
}


