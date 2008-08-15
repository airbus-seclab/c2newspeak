Newspeak Object output
----------------------
t004.adb
Global used

Global variables

Function definitions
t004() {
  do {
    [1-_int32]32 =(int32) !(4 + 3);
    [0-_float32]32 =(float32) !(4.5 +. 5.3);
  } with lbl0: {
  }
}



Newspeak output
---------------
t004.adb
t004() {
  (t004.adb:4#80)^[1-_int32]32 =(int32) 7;
  (t004.adb:5#95)^[0-_float32]32 =(float32) (4.5 +. 5.3);
}


