Warning: t097.ads:4#6: multiple definitions of global variable t097.a, in file t097.ads variable t097.a should probably be extern accepted
Newspeak output
---------------
int32 t096(void) {
  (t096.adb:3#9)^!return =(int32) 10;
}

void t097.piou(int32 x) {
  (t097.adb:11#11)^uint3 var;
  (t097.adb:13#12)^var =(uint3) 1;
}

void t097.proc(int32 z) {
  (t097.adb:17#8)^int32 x;
  (t097.adb:20#8)^choose {
   -->
    (t097.adb:20#8)^guard(t097.a_uint1);
    (t097.adb:23#13)^x =(int32) 2;
   -->
    (t097.adb:20#8)^guard(! t097.a_uint1);
  }
  (t097.adb:25#11)^t099(0: int32);
  (t097.adb:26#9)^{
    int32 tmp_cir!1;
    (t097.adb:26#9)^tmp_cir!1: int32 <- t098.a();
    (t097.adb:26#9)^x =(int32) tmp_cir!1_int32;
  }
}

int32 t098.a(void) {
  (t098.adb:32#11)^!return =(int32) 8;
}

uint3 t098.g(void) {
  (t098.adb:13#12)^!return =(uint3) 1;
}

void t098.main(uint2 z) {
  (t098.adb:19#9)^int32 x;
  (t098.adb:21#10)^{
    int32 tmp_cir!0;
    (t098.adb:21#10)^tmp_cir!0: int32 <- t098.t096();
    (t098.adb:21#10)^x =(int32) tmp_cir!0_int32;
  }
  (t098.adb:22#11)^t099(0: int32);
}

void t098.proc(int32 z) {
  (t098.adb:27#11)^t098.main(3: uint2);
}

int32 t098.t096(void) {
  (t098.adb:8#12)^!return =(int32) 5;
}

void t099(int32 x) {
}

uint1 t097.a;

