Newspeak Object output
----------------------
t026.adb
Global used

Global variables

Function definitions
t026() {
  do {
    int32 x;
    int32 y;
    1- =(int32) 10;
    0- =(int32) 0;
    do {
      while (1) {
        choose {
        --> assert((0 > 1-_int32));
            goto lbl2;
        --> assert((1-_int32 > 0));
        }
        0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1-_int32);
        1- =(int32) coerce[-2147483648,2147483647] (1-_int32 - 1);
      }
    } with lbl2: {
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t026.adb
t026() {
  (t026.adb:3#61)^int32 x;
  (t026.adb:4#83)^int32 y;
  (t026.adb:3#61)^1- =(int32) 10;
  (t026.adb:4#83)^0- =(int32) 0;
  (t026.adb:6#110)^do {
    (t026.adb:6#110)^while (1) {
      (t026.adb:6#110)^choose {
        | ! (1-_int32 > 0) -->
          (t026.adb:6#110)^goto lbl2;
        | (1-_int32 > 0) -->
      }
      (t026.adb:7#131)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1-_int32);
      (t026.adb:8#149)^1- =(int32) coerce[-2147483648,2147483647] (1-_int32 - 1);
    }
  } with lbl2: {
  }
}


