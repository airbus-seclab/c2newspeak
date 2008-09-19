Newspeak Object output
----------------------
t063.adb
Global used

Global variables

Function definitions
t063() {
  do {
    float32 a;
    uint2 c;
    int32 d;
    2- =(float32) 3.5;
    1- =(uint2) 1;
    0- =(int32) belongs[-2147483648,2147483648-1] 5;
  } with lbl0: {
  }
}



Newspeak output
---------------
t063.adb
t063() {
  (t063.adb:5#114)^float32 a;
  (t063.adb:7#168)^uint2 c;
  (t063.adb:8#196)^int32 d;
  (t063.adb:5#114)^2- =(float32) 3.5;
  (t063.adb:7#168)^1- =(uint2) 1;
  (t063.adb:8#196)^0- =(int32) 5;
}


