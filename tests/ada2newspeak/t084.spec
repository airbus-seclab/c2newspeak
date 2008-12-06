Newspeak Object output
----------------------
t084.adb
Global used

Global variables

Function definitions
t084() {
  do {
    int32 a;
    uint1 i;
    uint1 j;
    uint1 f;
    3- =(int32) belongs[-2147483648,2147483648-1] 3;
    2- =(uint1) (3-_int32 ==_int32 5);
    1- =(uint1) (coerce[-2147483648,2147483647] (3-_int32 + 5) ==_int32 7);
    0- =(uint1) (0 ==_uint2 1);
  } with lbl0: {
  }
}



Newspeak output
---------------
t084.adb
void t084(void) {
  (t084.adb:2#18)^int32 a;
  (t084.adb:6#93)^uint1 i;
  (t084.adb:7#126)^uint1 j;
  (t084.adb:11#185)^uint1 f;
  (t084.adb:2#18)^3- =(int32) 3;
  (t084.adb:6#93)^2- =(uint1) (3-_int32 ==_int32 5);
  (t084.adb:7#126)^1- =(uint1) (coerce[-2147483648,2147483647] (3-_int32 + 5) ==_int32 7);
  (t084.adb:11#185)^0- =(uint1) (0 ==_uint2 1);
}


