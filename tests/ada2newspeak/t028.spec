Newspeak Object output
----------------------
t028.adb
Global used

Global variables

Function definitions
t028() {
  do {
    int32 x;
    0- =(int32) belongs[-2147483648,2147483648-1] 0;
    do {
      while (1) {
                choose {
         -->
          guard((100 > 0-_int32));
          0- =(int32) belongs[-2147483648,2147483648-1] coerce[-2147483648,2147483647] (0-_int32 + 1);
         -->
          guard((0-_int32 > 100));
          0- =(int32) belongs[-2147483648,2147483648-1] coerce[-2147483648,2147483647] (0-_int32 - 100);
        }
      }
    } with lbl2: {
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t028.adb
void t028(void) {
  (t028.adb:2#18)^int32 x;
  (t028.adb:2#18)^0- =(int32) 0;
  (t028.adb:4#45)^while (1) {
    (t028.adb:5#53)^choose {
     -->
      (t028.adb:5#53)^guard((100 > 0-_int32));
      (t028.adb:6#74)^0- =(int32) belongs[-2147483648,2147483647] coerce[-2147483648,2147483647] (0-_int32 + 1);
     -->
      (t028.adb:5#53)^guard(! (100 > 0-_int32));
      (t028.adb:8#104)^0- =(int32) belongs[-2147483648,2147483647] coerce[-2147483648,2147483647] (0-_int32 - 100);
    }
  }
}


