Newspeak Object output
----------------------
t204.adb
Global used

Global variables

Function definitions
t204.oo() {
  do {
    int32 xn;
    int32 !tmp-1073741819;
    {
      int32 t204.add.arg1;
      0- =(int32) belongs[-2147483648,2147483648-1] 2;
      {
        int32 t204.add.arg2;
        0- =(int32) belongs[-2147483648,2147483648-1] 5;
        t204.add();
      }
    }
    1- =(int32) belongs[-2147483648,2147483648-1] 0-_int32;
  } with lbl0: {
  }
}


t204.add() {
  do {
    2- =(int32) belongs[-2147483648,2147483648-1] coerce[-2147483648,2147483647] (1-_int32 + 0-_int32);
    goto lbl0;
  } with lbl0: {
  }
}



Newspeak output
---------------
t204.adb
t204.add() {
  (t204.adb:6#130)^2- =(int32) belongs[-2147483648,2147483647] coerce[-2147483648,2147483647] (1-_int32 + 0-_int32);
}

t204.oo() {
  (t204.adb:10#182)^int32 xn;
  (t204.adb:12#211)^int32 !tmp-1073741819;
  (t204.adb:12#211)^{
    int32 t204.add.arg1;
    (t204.adb:12#211)^0- =(int32) 2;
    (t204.adb:12#211)^{
      int32 t204.add.arg2;
      (t204.adb:12#211)^0- =(int32) 5;
      (t204.adb:12#211)^t204.add();
    }
  }
  (t204.adb:12#211)^1- =(int32) 0-_int32;
}


