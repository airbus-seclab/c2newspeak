Newspeak Object output
----------------------
t109.adb
Global used
t109a.x

Global variables
extern int32 t109a.x;

Function definitions
t109() {
  do {
    int32 y;
    0- =(int32) Global(t109a.x)_int32;
  } with lbl0: {
  }
}



Warning: extern not accepted: t109a.x in t109a.ads line 2
Newspeak output
---------------
t109.adb
t109() {
  (t109.adb:3#30)^int32 y;
  (t109.adb:3#30)^0- =(int32) t109a.x_int32;
}

int32 t109a.x = 0;
