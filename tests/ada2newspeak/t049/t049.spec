Newspeak Object output
----------------------
t049/t049.adb
Global used
t049.x

Global variables
int32 t049.a = {0: int32 0}
;
uint2 t049.x;

Function definitions
t049.main() {
  do {
    Global(t049.x) =(uint2) 0;
  } with lbl0: {
  }
}



Newspeak output
---------------
t049/t049.adb
t049.main() {
  (t049.adb:11#225)^t049.x =(uint2) 0;
}

uint2 t049.x = 0;

