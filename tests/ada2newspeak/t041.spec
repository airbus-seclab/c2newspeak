Newspeak Object output
----------------------
t041.adb
Global used
t041a.z

Global variables
extern uint1 t041a.z;

Function definitions
t041() {
  do {
    Global(t041a.z) =(uint1) 0;
  } with lbl0: {
  }
}



Warning: extern not accepted: t041a.z in t041a.ads line 3
Newspeak output
---------------
t041.adb
t041() {
  (t041.adb:7#100)^t041a.z =(uint1) 0;
}

uint1 t041a.z = 0;
