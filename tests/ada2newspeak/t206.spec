Newspeak Object output
----------------------
t206.adb
Global used

Global variables

Function definitions
t206.id() {
  do {
    1- =(int32) belongs[-2147483648,2147483648-1] 0-_int32;
    goto lbl0;
  } with lbl0: {
  }
}


t206.main() {
  do {
    int32 xn;
    {
      int32 !tmp-1073741819;
      {
        int32 t206.id.arg1;
        0- =(int32) belongs[-2147483648,2147483648-1] 2147483647;
        t206.id();
      }
      1- =(int32) belongs[-2147483648,2147483648-1] 0-_int32;
    }
    {
      int32 !tmp-1073741820;
      {
        int32 t206.id.arg1;
        0- =(int32) belongs[-2147483648,2147483648-1] 2147483648;
        t206.id();
      }
      1- =(int32) belongs[-2147483648,2147483648-1] 0-_int32;
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t206.adb
t206.id() {
  (t206.adb:6#77)^1- =(int32) 0-_int32;
}

t206.main() {
  (t206.adb:10#118)^int32 xn;
  (t206.adb:12#141)^{
    int32 !tmp-1073741819;
    (t206.adb:12#141)^{
      int32 t206.id.arg1;
      (t206.adb:12#141)^0- =(int32) 2147483647;
      (t206.adb:12#141)^t206.id();
    }
    (t206.adb:12#141)^1- =(int32) 0-_int32;
  }
  (t206.adb:14#167)^{
    int32 !tmp-1073741820;
    (t206.adb:14#167)^{
      int32 t206.id.arg1;
      (t206.adb:14#167)^0- =(int32) belongs[-2147483648,2147483647] 2147483648;
      (t206.adb:14#167)^t206.id();
    }
    (t206.adb:14#167)^1- =(int32) 0-_int32;
  }
}


