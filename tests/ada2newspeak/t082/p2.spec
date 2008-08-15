Newspeak Object output
----------------------
t082/p2.adb
Global used
p2.c

Global variables
uint2 p2.c = {0: uint2 1}
;

Function definitions
p2.main() {
  do {
    uint2 x;
    uint2 y;
    uint2 b;
    2- =(uint2) Global(p2.c)_uint2;
    0- =(uint2) 2;
    1- =(uint2) 0;
  } with lbl0: {
  }
}



Newspeak output
---------------
t082/p2.adb
p2.main() {
  (p2.adb:8#125)^uint2 x;
  (p2.adb:9#155)^uint2 y;
  (p2.adb:10#171)^uint2 b;
  (p2.adb:8#125)^2- =(uint2) p2.c_uint2;
  (p2.adb:10#171)^0- =(uint2) 2;
  (p2.adb:13#214)^1- =(uint2) 0;
}

uint2 p2.c = {0: uint2 1};

