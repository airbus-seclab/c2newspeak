Newspeak Object output
----------------------
t020.adb
Global used

Global variables

Function definitions
t020() {
  do {
    uint2 a;
    uint2 b;
    uint3 c;
    2- =(uint2) belongs[0,3-1] 0;
    1- =(uint2) belongs[0,4-1] 1;
    0- =(uint3) belongs[0,5-1] 3;
        choose {
     -->
      guard((belongs[0,4-1] 1 > 1));
      2- =(uint2) belongs[0,3-1] 0;
     -->
      guard((1 > belongs[0,4-1] 1));
            choose {
       -->
        guard((1 ==_uint2 1-_uint2));
        1- =(uint2) belongs[0,4-1] 3;
        0- =(uint3) belongs[0,5-1] 4;
       -->
        guard((1-_uint2 ==_uint2 1));
      }
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t020.adb
void t020(void) {
  (t020.adb:5#114)^uint2 a;
  (t020.adb:6#134)^uint2 b;
  (t020.adb:7#154)^uint3 c;
  (t020.adb:5#114)^2- =(uint2) 0;
  (t020.adb:6#134)^1- =(uint2) 1;
  (t020.adb:7#154)^0- =(uint3) 3;
  (t020.adb:9#180)^choose {
   -->
    (t020.adb:9#180)^guard(! (1 > 1));
    (t020.adb:10#208)^2- =(uint2) 0;
   -->
    (t020.adb:9#180)^guard((1 > 1));
    (t020.adb:11#223)^choose {
     -->
      (t020.adb:11#223)^guard((1 ==_uint2 1-_uint2));
      (t020.adb:12#244)^1- =(uint2) 3;
      (t020.adb:13#259)^0- =(uint3) 4;
     -->
      (t020.adb:11#223)^guard(! (1 ==_uint2 1-_uint2));
    }
  }
}


