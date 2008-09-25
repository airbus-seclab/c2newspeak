Newspeak Object output
----------------------
t064.adb
Global used

Global variables

Function definitions
t064() {
  do {
    float32 a;
    uint2 c;
    int32 d;
    int32 x;
    3- =(float32) 3.5;
    2- =(uint2) belongs[0,3-1] 1;
    1- =(int32) belongs[-2147483648,2147483648-1] 5;
    0- =(int32) belongs[-2147483648,2147483648-1] 5;
  } with lbl0: {
  }
}



Newspeak output
---------------
t064.adb
t064() {
  (t064.adb:5#116)^float32 a;
  (t064.adb:7#168)^uint2 c;
  (t064.adb:8#196)^int32 d;
  (t064.adb:11#248)^int32 x;
  (t064.adb:5#116)^3- =(float32) 3.5;
  (t064.adb:7#168)^2- =(uint2) 1;
  (t064.adb:8#196)^1- =(int32) 5;
  (t064.adb:13#269)^0- =(int32) 5;
}


