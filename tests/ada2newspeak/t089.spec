Newspeak Object output
----------------------
t089.adb
Global used

Global variables

Function definitions
t089.jourdusoleil() {
  do {
    0- =(uint3) belongs[4,7-1] 5;
    goto lbl0;
  } with lbl0: {
  }
}


t089.a() {
  do {
    0- =(int32) belongs[10,16-1] 12;
    goto lbl0;
  } with lbl0: {
  }
}


t089.b() {
  do {
    0- =(int32) belongs[0,6-1] 2;
    goto lbl0;
  } with lbl0: {
  }
}


t089.main() {
  do {
    int32 x;
    uint3 y;
    int32 z;
    uint3 w;
    {
      int32 !tmp-1073741814;
      {
        int32 value_of_t089.b;
        t089.b();
        1- =(int32) 0-_int32;
      }
      4- =(int32) belongs[-2147483648,2147483648-1] 0-_int32;
    }
    2- =(uint3) belongs[5,7-1] 5;
    {
      int32 !tmp-1073741815;
      {
        int32 value_of_t089.a;
        t089.a();
        1- =(int32) 0-_int32;
      }
      2- =(int32) belongs[10,16-1] 0-_int32;
    }
    {
      uint3 !tmp-1073741816;
      {
        uint3 value_of_t089.jourdusoleil;
        t089.jourdusoleil();
        1- =(uint3) 0-_uint3;
      }
      1- =(uint3) belongs[0,7-1] 0-_uint3;
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t089.adb
t089.a() {
  (t089.adb:25#509)^0- =(int32) 12;
}

t089.b() {
  (t089.adb:20#446)^0- =(int32) 2;
}

t089.jourdusoleil() {
  (t089.adb:15#367)^0- =(uint3) 5;
}

t089.main() {
  (t089.adb:30#559)^int32 x;
  (t089.adb:31#578)^uint3 y;
  (t089.adb:32#597)^int32 z;
  (t089.adb:33#611)^uint3 w;
  (t089.adb:35#636)^{
    int32 !tmp-1073741814;
    (t089.adb:35#636)^{
      int32 value_of_t089.b;
      (t089.adb:35#636)^t089.b();
      (t089.adb:35#636)^1- =(int32) 0-_int32;
    }
    (t089.adb:35#636)^4- =(int32) 0-_int32;
  }
  (t089.adb:36#650)^2- =(uint3) 5;
  (t089.adb:37#669)^{
    int32 !tmp-1073741815;
    (t089.adb:37#669)^{
      int32 value_of_t089.a;
      (t089.adb:37#669)^t089.a();
      (t089.adb:37#669)^1- =(int32) 0-_int32;
    }
    (t089.adb:37#669)^2- =(int32) belongs[10,15] 0-_int32;
  }
  (t089.adb:38#683)^{
    uint3 !tmp-1073741816;
    (t089.adb:38#683)^{
      uint3 value_of_t089.jourdusoleil;
      (t089.adb:38#683)^t089.jourdusoleil();
      (t089.adb:38#683)^1- =(uint3) 0-_uint3;
    }
    (t089.adb:38#683)^1- =(uint3) belongs[0,6] 0-_uint3;
  }
}


