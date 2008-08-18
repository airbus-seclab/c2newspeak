Newspeak Object output
----------------------
t066.adb
Global used

Global variables

Function definitions
t066() {
  do {
    int32 x;
    0- =(int32) 5;
    0- =(int32) ((((0-_int32 + 10) - 5) - 2) - 3);
  } with lbl0: {
  }
}



Newspeak output
---------------
t066.adb
t066() {
  (t066.adb:4#100)^int32 x;
  (t066.adb:4#100)^0- =(int32) 5;
  (t066.adb:6#121)^0- =(int32) ((((0-_int32 + 10) - 5) - 2) - 3);
}


