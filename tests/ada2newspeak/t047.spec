Newspeak Object output
----------------------
t047.adb
Global used

Global variables

Function definitions
t047() {
  do {
    uint1 !tmp-1073741823;
    {
      int32 t094.z.arg1;
      0- =(int32) belongs[-2147483648,2147483648-1] 42;
      t094.z();
    }
        choose {
     -->
      guard(0-_uint1);
     -->
      guard(! 0-_uint1);
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t047.adb
void t047(void) {
  (t047.adb:7#140)^uint1 !tmp-1073741823;
  (t047.adb:7#140)^{
    int32 t094.z.arg1;
    (t047.adb:7#140)^0- =(int32) 42;
    (t047.adb:7#140)^t094.z();
  }
  (t047.adb:7#140)^choose {
   -->
    (t047.adb:7#140)^guard(0-_uint1);
   -->
    (t047.adb:7#140)^guard(! 0-_uint1);
  }
}


