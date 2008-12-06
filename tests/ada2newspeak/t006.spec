Newspeak Object output
----------------------
t006.adb
Global used

Global variables

Function definitions
t006() {
  do {
    int32 x;
    int32 y;
    int32 d;
    uint1 b;
    int32 z1;
    int32 z2;
    2- =(uint1) 1;
    1- =(int32) belongs[-2147483648,2147483648-1] 2;
    0- =(int32) belongs[-2147483648,2147483648-1] 2;
    3- =(int32) belongs[-2147483648,2147483648-1] 1;
  } with lbl0: {
  }
}



Newspeak output
---------------
t006.adb
void t006(int32) {
  (t006.adb:2#31)^int32 x;
  (t006.adb:2#31)^int32 y;
  (t006.adb:3#49)^int32 d;
  (t006.adb:4#65)^uint1 b;
  (t006.adb:5#89)^int32 z1;
  (t006.adb:5#89)^int32 z2;
  (t006.adb:4#65)^2- =(uint1) 1;
  (t006.adb:5#89)^1- =(int32) 2;
  (t006.adb:5#89)^0- =(int32) 2;
  (t006.adb:7#121)^3- =(int32) 1;
}


