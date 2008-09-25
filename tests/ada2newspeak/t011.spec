Newspeak Object output
----------------------
t011.adb
Global used

Global variables

Function definitions
t011() {
  do {
    uint2 a;
    uint2 b;
    uint3 c;
    2- =(uint2) belongs[0,3-1] 0;
    1- =(uint2) belongs[0,4-1] 2;
    0- =(uint3) belongs[0,5-1] 3;
    2- =(uint2) belongs[0,3-1] 2;
    1- =(uint2) belongs[0,4-1] 3;
    0- =(uint3) belongs[0,5-1] 4;
  } with lbl0: {
  }
}



Newspeak output
---------------
t011.adb
t011() {
  (t011.adb:6#126)^uint2 a;
  (t011.adb:7#146)^uint2 b;
  (t011.adb:8#166)^uint3 c;
  (t011.adb:6#126)^2- =(uint2) 0;
  (t011.adb:7#146)^1- =(uint2) 2;
  (t011.adb:8#166)^0- =(uint3) 3;
  (t011.adb:10#192)^2- =(uint2) 2;
  (t011.adb:11#204)^1- =(uint2) 3;
  (t011.adb:12#216)^0- =(uint3) 4;
}


