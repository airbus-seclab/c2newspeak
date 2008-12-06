Newspeak Object output
----------------------
t037.adb
Global used

Global variables

Function definitions
t037.appelfonction() {
  do {
    uint1 x;
    {
      uint1 !tmp-1073741820;
      {
        int32 t037.f.arg1;
        0- =(int32) belongs[-2147483648,2147483648-1] 10;
        t037.f();
      }
      1- =(uint1) belongs[0,2-1] 0-_uint1;
    }
    0- =(uint1) belongs[0,2-1] 1;
  } with lbl0: {
  }
}


t037.f() {
  do {
    1- =(uint1) belongs[0,2-1] 0;
    goto lbl0;
  } with lbl0: {
  }
}



Newspeak output
---------------
t037.adb
void t037.appelfonction(void) {
  (t037.adb:9#129)^uint1 x;
  (t037.adb:12#155)^{
    uint1 !tmp-1073741820;
    (t037.adb:12#155)^{
      int32 t037.f.arg1;
      (t037.adb:12#155)^0- =(int32) 10;
      (t037.adb:12#155)^t037.f();
    }
    (t037.adb:12#155)^1- =(uint1) 0-_uint1;
  }
  (t037.adb:13#173)^0- =(uint1) 1;
}

uint1 t037.f(int32) {
  (t037.adb:5#72)^1- =(uint1) 0;
}


