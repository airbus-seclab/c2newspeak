Newspeak output
---------------
t097.adb
void t097.piou(int32) {
  (t097.adb:8#94)^uint3 var;
  (t097.adb:10#121)^0- =(uint3) 1;
}

void t097.proc(int32) {
  (t097.adb:14#185)^int32 x;
  (t097.adb:17#213)^choose {
   -->
    (t097.adb:17#213)^guard(t097.a_uint1);
    (t097.adb:19#237)^0- =(int32) 2;
   -->
    (t097.adb:17#213)^guard(! t097.a_uint1);
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

