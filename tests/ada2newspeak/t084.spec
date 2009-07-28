Newspeak output
---------------
t084.adb
void t084(void) {
  (t084.adb:2#6)^int32 a;
  (t084.adb:6#6)^uint1 i;
  (t084.adb:7#6)^uint1 j;
  (t084.adb:11#6)^uint1 f;
  (t084.adb:2#6)^3- =(int32) 3;
  (t084.adb:6#6)^2- =(uint1) (3-_int32 ==_int32 5);
  (t084.adb:7#6)^1- =(uint1) (belongs[-2147483648,2147483647] (3-_int32 + 5) ==_int32 7);
  (t084.adb:11#6)^0- =(uint1) (0 ==_uint2 1);
}


