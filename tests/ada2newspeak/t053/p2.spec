Newspeak Object output
----------------------
t053/p2.adb
Global used

Global variables
extern uint1 p1.a;

Function definitions
p2.main() {
  do {
    int32 x;
    {
      int32 value_of_p2.a;
      p2.a();
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


p2.a() {
  do {
    0- =(int32) 5;
    goto lbl0;
  } with lbl0: {
  }
}


p2.g() {
  do {
    0- =(uint3) 1;
    goto lbl0;
  } with lbl0: {
  }
}


p2.proc() {
  do {
    uint2 p2.main.arg1;
    0- =(uint2) 3;
    p2.main();
  } with lbl0: {
  }
}



Newspeak output
---------------
t053/p2.adb
p2.a() {
  (p2.adb:8#93)^0- =(int32) 5;
}

p2.g() {
  (p2.adb:13#161)^0- =(uint3) 1;
}

p2.main() {
  (p2.adb:19#246)^int32 x;
  (p2.adb:21#274)^{
    int32 value_of_p2.a;
    (p2.adb:21#274)^p2.a();
    (p2.adb:21#274)^1- =(int32) 0-_int32;
  }
  (p2.adb:22#288)^{
    int32 p.arg1;
    (p2.adb:22#288)^0- =(int32) 0;
    (p2.adb:22#288)^p();
  }
}

p2.proc() {
  (p2.adb:27#358)^uint2 p2.main.arg1;
  (p2.adb:27#358)^0- =(uint2) 3;
  (p2.adb:27#358)^p2.main();
}


