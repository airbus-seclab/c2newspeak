Newspeak output
---------------
void t461.a(void) {
  (t461.adb:12#11)^{ int32 32; int32 0; }64 value_of!0!m;
  (t461.adb:12#11)^value_of!0!m: { int32 32; int32 0; }64 <- t461.m(belongs[1,2] t461.x_int32: int32);
  (t461.adb:12#11)^t461.r =(int32) value_of!0!m + 32_int32;
}

{ int32 32; int32 0; }64 t461.m(int32 y) {
  (t461.adb:8#12)^!return =64 t461.mi + ((belongs[1,2] y_int32 - 1) * 64);
}

{ int32 32; int32 0; }64[2] t461.mi;
int32 t461.r;
int32 t461.x;
(t461.adb:3#7)^t461.x =(int32) 1;

