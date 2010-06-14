Newspeak output
---------------
t293.adb
int32 t293(int32 x) {
  (t293.adb:8#10)^uint1 tmp0;
  (t293.adb:8#10)^choose {
   -->
    (t293.adb:8#10)^guard((x_int32 > 0));
    (t293.adb:8#10)^tmp0 =(int32) x_int32;
   -->
    (t293.adb:8#10)^guard(! (x_int32 > 0));
    (t293.adb:8#10)^tmp0 =(int32) belongs[-2147483648,2147483647] (0 - x_int32);
  }
  (t293.adb:8#10)^!return =(int32) tmp0_uint1;
}


