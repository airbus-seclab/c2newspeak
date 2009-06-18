Newspeak output
---------------
t097.adb
void t097.piou(int32) {
  (t097.adb:8#11)^uint3 var;
  (t097.adb:10#12)^0- =(uint3) 1;
}

void t097.proc(int32) {
  (t097.adb:14#8)^int32 x;
  (t097.adb:17#8)^choose {
   -->
    (t097.adb:17#8)^guard(t097.a_uint1);
    (t097.adb:19#13)^0- =(int32) 2;
   -->
    (t097.adb:17#8)^guard(! t097.a_uint1);
  }
  (t097.adb:21#10)^{
    int32 t099.arg1;
    (t097.adb:21#10)^0- =(int32) 0;
    (t097.adb:21#10)^t099();
  }
  (t097.adb:22#9)^{
    int32 !tmp0;
    (t097.adb:22#9)^t098.a();
    (t097.adb:22#9)^1- =(int32) 0-_int32;
  }
}

uint1 t097.a = 0;

