Newspeak Object output
----------------------
t046.adb
Global used

Global variables

Function definitions
t046() {
  do {
    uint1 !tmp-1073741823;
    {
      int32 t093.z.arg1;
      0- =(int32) belongs[-2147483648,2147483648-1] 42;
      t093.z();
    }
    choose {
    --> assert(0-_uint1);
    --> assert(! 0-_uint1);
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t046.adb
void t046(void) {
  (t046.adb:6#88)^uint1 !tmp-1073741823;
  (t046.adb:6#88)^{
    int32 t093.z.arg1;
    (t046.adb:6#88)^0- =(int32) 42;
    (t046.adb:6#88)^t093.z();
  }
  (t046.adb:6#88)^choose {
   -->
    (t046.adb:6#88)^guard(0-_uint1);
   -->
    (t046.adb:6#88)^guard(! 0-_uint1);
  }
}


