Newspeak Object output
----------------------
t053/p1.adb
Global used
p1.a

Global variables
uint1 p1.a;

Function definitions
p1.piou() {
  do {
    uint3 var;
    0- =(uint3) 1;
  } with lbl0: {
  }
}


p1.proc() {
  do {
    int32 x;
    choose {
    --> assert(Global(p1.a)_uint1);
        0- =(int32) 2;
    --> assert(! Global(p1.a)_uint1);
    }
    {
      int32 p.arg1;
      0- =(int32) 0;
      p();
    }
    {
      int32 value_of_p2.a;
      p2.a();
      1- =(int32) 0-_int32;
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t053/p1.adb
p1.piou() {
  (p1.adb:8#82)^uint3 var;
  (p1.adb:10#109)^0- =(uint3) 1;
}

p1.proc() {
  (p1.adb:14#173)^int32 x;
  (p1.adb:17#201)^choose {
    | p1.a_uint1 -->
      (p1.adb:19#225)^0- =(int32) 2;
    | ! p1.a_uint1 -->
  }
  (p1.adb:21#256)^{
    int32 p.arg1;
    (p1.adb:21#256)^0- =(int32) 0;
    (p1.adb:21#256)^p();
  }
  (p1.adb:22#268)^{
    int32 value_of_p2.a;
    (p1.adb:22#268)^p2.a();
    (p1.adb:22#268)^1- =(int32) 0-_int32;
  }
}

uint1 p1.a = 0;

