Newspeak Object output
----------------------
t098.adb
Global used

Global variables
extern uint1 t097.a;

Function definitions
t098.a() {
  do {
    0- =(int32) 5;
    goto lbl0;
  } with lbl0: {
  }
}


t098.g() {
  do {
    0- =(uint3) 1;
    goto lbl0;
  } with lbl0: {
  }
}


t098.main() {
  do {
    int32 x;
    {
      int32 value_of_t098.a;
      t098.a();
      1- =(int32) 0-_int32;
    }
    {
      int32 p.arg1;
      0- =(int32) 0;
      p();
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
t098.a() {
  (t098.adb:8#105)^0- =(int32) 5;
}

t098.g() {
  (t098.adb:13#175)^0- =(uint3) 1;
}

t098.main() {
  (t098.adb:19#266)^int32 x;
  (t098.adb:21#294)^{
    int32 value_of_t098.a;
    (t098.adb:21#294)^t098.a();
    (t098.adb:21#294)^1- =(int32) 0-_int32;
  }
  (t098.adb:22#308)^{
    int32 p.arg1;
    (t098.adb:22#308)^0- =(int32) 0;
    (t098.adb:22#308)^p();
  }
}

t098.proc() {
  (t098.adb:27#378)^uint2 t098.main.arg1;
  (t098.adb:27#378)^0- =(uint2) 3;
  (t098.adb:27#378)^t098.main();
}


