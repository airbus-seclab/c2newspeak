Newspeak output
---------------
t005.adb
void t005(ptr, ptr) {
  (t005.adb:4#6)^[1-_int32]32 =(int32) belongs[-2147483648,2147483647] (0 - (2 * [1-_int32]32_ptr));
  (t005.adb:5#6)^[1-_int32]32 =(int32) -2;
  (t005.adb:7#6)^[1-_int32]32 =(int32) -7;
  (t005.adb:9#6)^[1-_int32]32 =(int32) belongs[-2147483648,2147483647] ((0 + (5 % 2)) - (-3 * (5 % 4)));
  (t005.adb:10#6)^[0-_uint1]1 =(uint1) (5 > [1-_int32]32_ptr);
  (t005.adb:11#6)^[0-_uint1]1 =(uint1) ([1-_int32]32_ptr ==_int32 5);
  (t005.adb:12#6)^[0-_uint1]1 =(uint1) ! ([1-_int32]32_ptr > 2);
  (t005.adb:13#6)^[0-_uint1]1 =(uint1) ! (2 ==_int32 3);
  (t005.adb:14#6)^[0-_uint1]1 =(uint1) (6 > [1-_int32]32_ptr);
  (t005.adb:15#6)^[0-_uint1]1 =(uint1) ! ([1-_int32]32_ptr > 6);
}


