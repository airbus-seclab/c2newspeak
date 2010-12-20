Newspeak output
---------------
void t005(int32 x, uint1 y) {
  (t005.adb:4#6)^x =(int32) belongs[-2147483648,2147483647] (0 - belongs[-2147483648,2147483647] (2 * x_int32));
  (t005.adb:5#6)^x =(int32) -2;
  (t005.adb:7#6)^x =(int32) -7;
  (t005.adb:9#6)^x =(int32) belongs[-2147483648,2147483647] ((0 + (5 % 2)) - (-3 * (5 % 4)));
  (t005.adb:10#6)^y =(uint1) (5 > x_int32);
  (t005.adb:11#6)^y =(uint1) (x_int32 ==_int32 5);
  (t005.adb:12#6)^y =(uint1) ! (x_int32 > 2);
  (t005.adb:13#6)^y =(uint1) ! (2 ==_int32 3);
  (t005.adb:14#6)^y =(uint1) (6 > x_int32);
  (t005.adb:15#6)^y =(uint1) ! (x_int32 > 6);
}


