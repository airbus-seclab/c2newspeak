Newspeak Object output
----------------------
t057.adb
Global used
t057.a

Global variables
uint1 t057.a;

Function definitions
t057.main() {
  do {
    uint1 x;
    uint1 y;
    1- =(uint1) belongs[0,2-1] 0;
    0- =(uint1) Global(t057.a)_uint1;
  } with lbl0: {
  }
}



Newspeak output
---------------
t057.adb
void t057.main(void) {
  (t057.adb:8#89)^uint1 x;
  (t057.adb:9#113)^uint1 y;
  (t057.adb:8#89)^1- =(uint1) 0;
  (t057.adb:9#113)^0- =(uint1) t057.a_uint1;
}

uint1 t057.a = 0;

