Newspeak Object output
----------------------
t023.adb
Global used

Global variables

Function definitions
t023() {
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
          0- =(uint1) ! 2-_uint1;
      --> assert(! 3-_uint1);
          0- =(uint1) 2-_uint1;
      }
      1- =(uint1) 0-_uint1;
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t023.adb
void t023(void) {
  (t023.adb:3#33)^uint1 b1;
  (t023.adb:3#33)^uint1 b2;
  (t023.adb:4#62)^uint1 b3;
  (t023.adb:3#33)^2- =(uint1) 1;
  (t023.adb:3#33)^1- =(uint1) 1;
  (t023.adb:6#85)^{
    uint1 tmp0;
    (t023.adb:6#85)^choose {
     -->
      (t023.adb:6#85)^guard(3-_uint1);
      (t023.adb:6#85)^0- =(uint1) ! 2-_uint1;
     -->
      (t023.adb:6#85)^guard(! 3-_uint1);
      (t023.adb:6#85)^0- =(uint1) 2-_uint1;
    }
    (t023.adb:6#85)^1- =(uint1) 0-_uint1;
  }
}


