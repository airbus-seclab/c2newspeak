Newspeak Object output
----------------------
t032.adb
Global used
t032.x

Global variables
int32 t032.x;

Function definitions
t032.proc() {
  do {
    Global(t032.x) =(int32) belongs[-2147483648,2147483648-1] coerce[-2147483648,2147483647] (Global(t032.x)_int32 + 2);
  } with lbl0: {
  }
}



Newspeak output
---------------
t032.adb
t032.proc() {
  (t032.adb:5#52)^t032.x =(int32) belongs[-2147483648,2147483647] coerce[-2147483648,2147483647] (t032.x_int32 + 2);
}

int32 t032.x = 0;

