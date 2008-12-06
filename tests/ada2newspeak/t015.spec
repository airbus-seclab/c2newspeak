Newspeak Object output
----------------------
t015.adb
Global used

Global variables

Function definitions
t015() {
  do {
    int32 x;
    0- =(int32) belongs[-2147483648,2147483648-1] 10;
    choose {
    --> assert((2 > 0-_int32));
        0- =(int32) belongs[-2147483648,2147483648-1] coerce[-2147483648,2147483647] (0-_int32 + 1);
    --> assert((0-_int32 > 2));
        0- =(int32) belongs[-2147483648,2147483648-1] coerce[-2147483648,2147483647] (0-_int32 - 1);
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t015.adb
void t015(void) {
  (t015.adb:2#18)^int32 x;
  (t015.adb:4#38)^0- =(int32) 10;
  (t015.adb:5#48)^choose {
    | (2 > 0-_int32) -->
      (t015.adb:7#67)^0- =(int32) belongs[-2147483648,2147483647] coerce[-2147483648,2147483647] (0-_int32 + 1);
    | ! (2 > 0-_int32) -->
      (t015.adb:9#89)^0- =(int32) belongs[-2147483648,2147483647] coerce[-2147483648,2147483647] (0-_int32 - 1);
  }
}


