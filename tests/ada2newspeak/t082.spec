Newspeak Object output
----------------------
t082.adb
Global used
t082.c

Global variables
uint2 t082.c = {0: uint2 1}
;

Function definitions
t082.main() {
  do {
    uint2 x;
    uint2 y;
    uint2 b;
    2- =(uint2) Global(t082.c)_uint2;
    0- =(uint2) 2;
    1- =(uint2) 0;
  } with lbl0: {
  }
}



Newspeak output
---------------
t082.adb
t082.main() {
  (t082.adb:8#133)^uint2 x;
  (t082.adb:9#163)^uint2 y;
  (t082.adb:10#179)^uint2 b;
  (t082.adb:8#133)^2- =(uint2) t082.c_uint2;
  (t082.adb:10#179)^0- =(uint2) 2;
  (t082.adb:13#225)^1- =(uint2) 0;
}

uint2 t082.c = {0: uint2 1};

