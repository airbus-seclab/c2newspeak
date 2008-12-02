Newspeak Object output
----------------------
t098.adb
Global used

Global variables
uint1 t097.a;

Function definitions
t098.t096() {
  do {
    0- =(int32) belongs[-2147483648,2147483648-1] 5;
    goto lbl0;
  } with lbl0: {
  }
}


t098.g() {
  do {
    0- =(uint3) belongs[0,5-1] 1;
    goto lbl0;
  } with lbl0: {
  }
}


t098.main() {
  do {
    int32 x;
    {
      int32 !tmp-1073741817;
      t098.t096();
      1- =(int32) belongs[-2147483648,2147483648-1] 0-_int32;
    }
    {
      int32 t099.arg1;
      0- =(int32) 0;
      t099();
    }
  } with lbl0: {
  }
}


t098.proc() {
  do {
    uint2 t098.main.arg1;
    0- =(uint2) 3;
    t098.main();
  } with lbl0: {
  }
}



Newspeak output
---------------
t098.adb
t098.g() {
  (t098.adb:13#181)^0- =(uint3) 1;
}

t098.main() {
  (t098.adb:19#272)^int32 x;
  (t098.adb:21#300)^{
    int32 !tmp-1073741817;
    (t098.adb:21#300)^t098.t096();
    (t098.adb:21#300)^1- =(int32) 0-_int32;
  }
  (t098.adb:22#317)^{
    int32 t099.arg1;
    (t098.adb:22#317)^0- =(int32) 0;
    (t098.adb:22#317)^t099();
  }
}

t098.proc() {
  (t098.adb:27#390)^uint2 t098.main.arg1;
  (t098.adb:27#390)^0- =(uint2) 3;
  (t098.adb:27#390)^t098.main();
}

t098.t096() {
  (t098.adb:8#108)^0- =(int32) 5;
}


