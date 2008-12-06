Newspeak Object output
----------------------
t007.adb
Global used

Global variables

Function definitions
t007() {
  do {
    int32 x;
    int32 y;
    int32 d;
    uint1 b;
    int32 z1;
    int32 z2;
    5- =(int32) belongs[-2147483648,2147483648-1] 6-_int32;
    4- =(int32) belongs[-2147483648,2147483648-1] 6-_int32;
    2- =(uint1) 1;
    1- =(int32) belongs[-2147483648,2147483648-1] 4-_int32;
    0- =(int32) belongs[-2147483648,2147483648-1] 4-_int32;
    3- =(int32) belongs[-2147483648,2147483648-1] 1;
  } with lbl0: {
  }
}



Newspeak output
---------------
t007.adb
void t007(int32) {
  (t007.adb:3#59)^int32 x;
  (t007.adb:3#59)^int32 y;
  (t007.adb:4#82)^int32 d;
  (t007.adb:5#98)^uint1 b;
  (t007.adb:6#122)^int32 z1;
  (t007.adb:6#122)^int32 z2;
  (t007.adb:3#59)^5- =(int32) 6-_int32;
  (t007.adb:3#59)^4- =(int32) 6-_int32;
  (t007.adb:5#98)^2- =(uint1) 1;
  (t007.adb:6#122)^1- =(int32) 4-_int32;
  (t007.adb:6#122)^0- =(int32) 4-_int32;
  (t007.adb:8#154)^3- =(int32) 1;
}


