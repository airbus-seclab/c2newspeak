Newspeak Object output
----------------------
t210.adb
Global used

Global variables

Function definitions
t210.f() {
  do {
    1- =(int32) belongs[-2147483648,2147483648-1] 1;
    goto lbl0;
  } with lbl0: {
  }
}


t210.main() {
  do {
    int32 x;
    int32 !tmp-1073741820;
    {
      int32 t210.f.arg1;
      0- =(int32) belongs[-2147483648,2147483648-1] 2;
      t210.f();
    }
    1- =(int32) belongs[-2147483648,2147483648-1] 0-_int32;
  } with lbl0: {
  }
}



Newspeak output
---------------
t210.adb
t210.f() {
  (t210.adb:5#74)^1- =(int32) 1;
}

t210.main() {
  (t210.adb:9#122)^int32 x;
  (t210.adb:11#150)^int32 !tmp-1073741820;
  (t210.adb:11#150)^{
    int32 t210.f.arg1;
    (t210.adb:11#150)^0- =(int32) 2;
    (t210.adb:11#150)^t210.f();
  }
  (t210.adb:11#150)^1- =(int32) 0-_int32;
}


