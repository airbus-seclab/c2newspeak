Newspeak Object output
----------------------
t100.adb
Global used

Global variables

Function definitions
t100() {
  do {
    1- =(int32) belongs[-2147483648,2147483648-1] coerce[-2147483648,2147483647] (2 * 0-_int32);
    goto lbl0;
  } with lbl0: {
  }
}



Newspeak output
---------------
t100.adb
int32 t100(int32) {
  (t100.adb:3#50)^1- =(int32) belongs[-2147483648,2147483647] coerce[-2147483648,2147483647] (2 * 0-_int32);
}


