Newspeak Object output
----------------------
t097.adb
Global used
t097.a

Global variables
uint1 t097.a;

Function definitions
t097.piou() {
  do {
    uint3 var;
    0- =(uint3) belongs[0,5-1] 1;
  } with lbl0: {
  }
}


t097.proc() {
  do {
    int32 x;
    choose {
    --> assert(Global(t097.a)_uint1);
        0- =(int32) belongs[-2147483648,2147483648-1] 2;
    --> assert(! Global(t097.a)_uint1);
    }
    {
      int32 t099.arg1;
      0- =(int32) 0;
      t099();
    }
    {
      int32 !tmp-1073741818;
      t098.a();
      1- =(int32) belongs[-2147483648,2147483648-1] 0-_int32;
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t097.adb
t097.piou() {
  (t097.adb:8#94)^uint3 var;
  (t097.adb:10#121)^0- =(uint3) 1;
}

t097.proc() {
  (t097.adb:14#185)^int32 x;
  (t097.adb:17#213)^choose {
    | t097.a_uint1 -->
      (t097.adb:19#237)^0- =(int32) 2;
    | ! t097.a_uint1 -->
  }
  (t097.adb:21#268)^{
    int32 t099.arg1;
    (t097.adb:21#268)^0- =(int32) 0;
    (t097.adb:21#268)^t099();
  }
  (t097.adb:22#283)^{
    int32 !tmp-1073741818;
    (t097.adb:22#283)^t098.a();
    (t097.adb:22#283)^1- =(int32) 0-_int32;
  }
}

uint1 t097.a = 0;

