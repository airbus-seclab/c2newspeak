Newspeak Object output
----------------------
t027.adb
Global used

Global variables

Function definitions
t027() {
  do {
    int32 x;
    int32 y;
    1- =(int32) belongs[-2147483648,2147483648-1] 10;
    0- =(int32) belongs[-2147483648,2147483648-1] 0;
    do {
      while (1) {
        0- =(int32) belongs[-2147483648,2147483648-1] coerce[-2147483648,2147483647] (0-_int32 + 1-_int32);
        1- =(int32) belongs[-2147483648,2147483648-1] coerce[-2147483648,2147483647] (1-_int32 - 1);
        choose {
        --> assert((0 > 1-_int32));
            goto lbl2;
        --> assert((1-_int32 > 0));
        }
      }
    } with lbl2: {
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t027.adb
t027() {
  (t027.adb:3#62)^int32 x;
  (t027.adb:4#84)^int32 y;
  (t027.adb:3#62)^1- =(int32) 10;
  (t027.adb:4#84)^0- =(int32) 0;
  (t027.adb:6#111)^do {
    (t027.adb:6#111)^while (1) {
      (t027.adb:7#119)^0- =(int32) belongs[-2147483648,2147483647] coerce[-2147483648,2147483647] (0-_int32 + 1-_int32);
      (t027.adb:8#137)^1- =(int32) belongs[-2147483648,2147483647] coerce[-2147483648,2147483647] (1-_int32 - 1);
      (t027.adb:9#153)^choose {
        | (0 > 1-_int32) -->
          (t027.adb:9#153)^goto lbl2;
        | ! (0 > 1-_int32) -->
      }
    }
  } with lbl2: {
  }
}


