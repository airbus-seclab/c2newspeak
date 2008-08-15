Newspeak Object output
----------------------
t017.adb
Global used

Global variables

Function definitions
t017() {
  do {
    uint2 a;
    uint2 b;
    uint3 c;
    2- =(uint2) 0;
    1- =(uint2) 1;
    0- =(uint3) 3;
    choose {
    --> assert((1 > 0));
        2- =(uint2) 0;
    --> assert(!(0 > 1));
        choose {
        --> assert(!(1 ==_uint2 1-_uint2));
            1- =(uint2) 3;
            0- =(uint3) 4;
        --> assert((1-_uint2 ==_uint2 1));
        }
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t017.adb
t017() {
  (t017.adb:5#114)^uint2 a;
  (t017.adb:6#134)^uint2 b;
  (t017.adb:7#154)^uint3 c;
  (t017.adb:5#114)^2- =(uint2) 0;
  (t017.adb:6#134)^1- =(uint2) 1;
  (t017.adb:7#154)^0- =(uint3) 3;
  (t017.adb:9#180)^choose {
    | ! (0 > 1) -->
      (t017.adb:10#200)^2- =(uint2) 0;
    | (0 > 1) -->
      (t017.adb:11#215)^choose {
        | (1 ==_uint2 1-_uint2) -->
          (t017.adb:12#236)^1- =(uint2) 3;
          (t017.adb:13#251)^0- =(uint3) 4;
        | ! (1 ==_uint2 1-_uint2) -->
      }
  }
}


