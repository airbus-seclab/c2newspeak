Newspeak Object output
----------------------
t071.adb
Global used

Global variables

Function definitions
proc() {
  do {
    uint1 x;
    uint1 y;
    uint1 z;
    2- =(uint1) 0;
    {
      uint1 value_of_t103.f;
      t103.f();
      2- =(uint1) 0-_uint1;
    }
    0- =(uint1) 1-_uint1;
  } with lbl0: {
  }
}



Newspeak output
---------------
t071.adb
proc() {
  (t071.adb:4#53)^uint1 x;
  (t071.adb:5#86)^uint1 y;
  (t071.adb:6#118)^uint1 z;
  (t071.adb:4#53)^2- =(uint1) 0;
  (t071.adb:5#86)^{
    uint1 value_of_t103.f;
    (t071.adb:5#86)^t103.f();
    (t071.adb:5#86)^2- =(uint1) 0-_uint1;
  }
  (t071.adb:6#118)^0- =(uint1) 1-_uint1;
}


