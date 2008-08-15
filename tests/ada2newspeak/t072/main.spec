Newspeak Object output
----------------------
t072/main.adb
Global used

Global variables

Function definitions
main() {
  do {
    int32 x;
    int32 y;
    0- =(int32) 18;
    1- =(int32) 0-_int32;
    0- =(int32) !(1-_int32 * 2);
  } with lbl0: {
  }
}



Newspeak output
---------------
t072/main.adb
main() {
  (main.adb:4#35)^int32 x;
  (main.adb:5#50)^int32 y;
  (main.adb:5#50)^0- =(int32) 18;
  (main.adb:7#77)^1- =(int32) 0-_int32;
  (main.adb:8#88)^0- =(int32) (1-_int32 * 2);
}


