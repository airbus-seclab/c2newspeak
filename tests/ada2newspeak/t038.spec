Newspeak Object output
----------------------
t038.adb
Global used

Global variables

Function definitions
t038.appel_fonction() {
  do {
    uint1 x;
    uint1 y;
    1- =(uint1) belongs[0,2-1] 0;
    {
      uint1 !tmp-1073741820;
      {
        uint1 value_of_t038.a;
        t038.a();
        1- =(uint1) 0-_uint1;
      }
      1- =(uint1) belongs[0,2-1] 0-_uint1;
    }
  } with lbl0: {
  }
}


t038.a() {
  do {
    0- =(uint1) belongs[0,2-1] 0;
    goto lbl0;
  } with lbl0: {
  }
}



Newspeak output
---------------
t038.adb
t038.a() {
  (t038.adb:5#61)^0- =(uint1) 0;
}

t038.appel_fonction() {
  (t038.adb:9#119)^uint1 x;
  (t038.adb:10#135)^uint1 y;
  (t038.adb:12#160)^1- =(uint1) 0;
  (t038.adb:13#174)^{
    uint1 !tmp-1073741820;
    (t038.adb:13#174)^{
      uint1 value_of_t038.a;
      (t038.adb:13#174)^t038.a();
      (t038.adb:13#174)^1- =(uint1) 0-_uint1;
    }
    (t038.adb:13#174)^1- =(uint1) 0-_uint1;
  }
}


