Newspeak Object output
----------------------
t033.adb
Global used

Global variables

Function definitions
t033.appelfonction() {
  do {
    int32 x;
    int32 value_of_t033.f;
    {
      int32 t033.f.arg1;
      0- =(int32) 2;
      t033.f();
    }
    1- =(int32) 0-_int32;
  } with lbl0: {
  }
}


t033.f() {
  do {
    1- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
    goto lbl0;
  } with lbl0: {
  }
}



Newspeak output
---------------
t033.adb
t033.appelfonction() {
  (t033.adb:11#184)^int32 x;
  (t033.adb:14#212)^int32 value_of_t033.f;
  (t033.adb:14#212)^{
    int32 t033.f.arg1;
    (t033.adb:14#212)^0- =(int32) 2;
    (t033.adb:14#212)^t033.f();
  }
  (t033.adb:14#212)^1- =(int32) 0-_int32;
}

t033.f() {
  (t033.adb:6#122)^1- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
}


