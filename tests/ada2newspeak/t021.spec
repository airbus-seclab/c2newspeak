Newspeak Object output
----------------------
t021.adb
Global used

Global variables

Function definitions
t021() {
  do {
    uint1 b1;
    uint1 b2;
    uint1 b3;
    2- =(uint1) 1;
    1- =(uint1) 1;
    {
      uint1 tmp0;
      choose {
      --> assert(3-_uint1);
          0- =(uint1) 2-_uint1;
      --> assert(! 3-_uint1);
          0- =(uint1) 0;
      }
      1- =(uint1) 0-_uint1;
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t021.adb
t021() {
  (t021.adb:3#33)^uint1 b1;
  (t021.adb:3#33)^uint1 b2;
  (t021.adb:4#62)^uint1 b3;
  (t021.adb:3#33)^2- =(uint1) 1;
  (t021.adb:3#33)^1- =(uint1) 1;
  (t021.adb:6#85)^{
    uint1 tmp0;
    (t021.adb:6#85)^choose {
      | 3-_uint1 -->
        (t021.adb:6#85)^0- =(uint1) 2-_uint1;
      | ! 3-_uint1 -->
        (t021.adb:6#85)^0- =(uint1) 0;
    }
    (t021.adb:6#85)^1- =(uint1) 0-_uint1;
  }
}

