Newspeak Object output
----------------------
t003.adb
Global used

Global variables

Function definitions
t003() {
  do {
    [1-_int32]32 =(int32) belongs[-2147483648,2147483648-1] 4;
    [0-_float32]32 =(float32) 4.5;
  } with lbl0: {
  }
}



Newspeak output
---------------
t003.adb
void t003(ptr, ptr) {
  (t003.adb:3#52)^[1-_int32]32 =(int32) 4;
  (t003.adb:4#63)^[0-_float32]32 =(float32) 4.5;
}


