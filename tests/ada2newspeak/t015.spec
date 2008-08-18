Newspeak Object output
----------------------
t015.adb
Global used

Global variables

Function definitions
t015() {
  do {
    int32 x;
    0- =(int32) 10;
    choose {
    --> assert((2 > 0-_int32));
        0- =(int32) (0-_int32 + 1);
    --> assert((0-_int32 > 2));
        0- =(int32) (0-_int32 - 1);
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t015.adb
t015() {
  (t015.adb:2#18)^int32 x;
  (t015.adb:4#38)^0- =(int32) 10;
  (t015.adb:5#48)^choose {
    | (2 > 0-_int32) -->
      (t015.adb:7#67)^0- =(int32) (0-_int32 + 1);
    | ! (2 > 0-_int32) -->
      (t015.adb:9#89)^0- =(int32) (0-_int32 - 1);
  }
}


