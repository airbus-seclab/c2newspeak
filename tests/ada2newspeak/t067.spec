Newspeak Object output
----------------------
t067.adb
Global used

Global variables

Function definitions
t067() {
  do {
    int32 x;
    int32 y;
    1- =(int32) belongs[0,101-1] 5;
    0- =(int32) belongs[0,51-1] 10;
    1- =(int32) belongs[0,101-1] ((1-_int32 + 20) - (2 * 0-_int32));
  } with lbl0: {
  }
}



Newspeak output
---------------
t067.adb
t067() {
  (t067.adb:5#132)^int32 x;
  (t067.adb:6#147)^int32 y;
  (t067.adb:5#132)^1- =(int32) 5;
  (t067.adb:6#147)^0- =(int32) 10;
  (t067.adb:8#170)^1- =(int32) belongs[0,100] ((1-_int32 + 20) - (2 * 0-_int32));
}


