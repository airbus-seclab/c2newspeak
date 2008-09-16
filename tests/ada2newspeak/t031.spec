Newspeak Object output
----------------------
t031.adb
Global used
t031.x

Global variables
int32 t031.x;

Function definitions
t031.proc() {
  do {
    Global(t031.x) =(int32) coerce[-2147483648,2147483647] (Global(t031.x)_int32 + 2);
  } with lbl0: {
  }
}



Newspeak output
---------------
t031.adb
t031.proc() {
  (t031.adb:6#97)^t031.x =(int32) coerce[-2147483648,2147483647] (t031.x_int32 + 2);
}

int32 t031.x = 0;

