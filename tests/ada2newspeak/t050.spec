Newspeak Object output
----------------------
t050.adb
Global used
t050.x

Global variables
int32 t050.a = {0: int32 0}
;
uint1 t050.x;

Function definitions
t050.main() {
  do {
    uint1 value_of_t095.a;
    t095.a();
    Global(t050.x) =(uint1) 0-_uint1;
  } with lbl0: {
  }
}



Newspeak output
---------------
t050.adb
t050.main() {
  (t050.adb:11#212)^uint1 value_of_t095.a;
  (t050.adb:11#212)^t095.a();
  (t050.adb:11#212)^t050.x =(uint1) 0-_uint1;
}

uint1 t050.x = 0;

