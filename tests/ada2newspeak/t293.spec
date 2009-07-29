Newspeak output
---------------
t293.adb
int32 t293(int32) {
  (t293.adb:8#10)^uint1 tmp0;
  (t293.adb:8#10)^choose {
   -->
    (t293.adb:8#10)^guard((1-_int32 > 0));
    (t293.adb:8#10)^0- =(int32) 1-_int32;
   -->
    (t293.adb:8#10)^guard(! (1-_int32 > 0));
    (t293.adb:8#10)^0- =(int32) belongs[-2147483648,2147483647] (0 - 1-_int32);
  }
  (t293.adb:8#10)^2- =(int32) 0-_uint1;
}


