Newspeak Object output
----------------------
t042/t042.adb
Global used
t042a.z

Global variables
extern uint1 t042a.z;

Function definitions
t042() {
  do {
    uint1 a;
    Global(t042a.z) =(uint1) 0;
    0- =(uint1) 1;
  } with lbl0: {
  }
}



Warning: extern not accepted: t042a.z in t042a.ads line 3
Newspeak output
---------------
t042/t042.adb
t042() {
  (t042.adb:6#175)^uint1 a;
  (t042.adb:9#203)^t042a.z =(uint1) 0;
  (t042.adb:10#214)^0- =(uint1) 1;
}

uint1 t042a.z = 0;

