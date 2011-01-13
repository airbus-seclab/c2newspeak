Newspeak output
---------------
void t460.a(void) {
  (t460.adb:12#11)^{ { int32 32; int32 0; }64 64; { int32 32; int32 0; }64 0; }128 value_of!0!m;
  (t460.adb:12#11)^value_of!0!m: { { int32 32; int32 0; }64 64; { int32 32; int32 0; }64 0; }128 <- t460.m(belongs[1,2] t460.x_int32: int32);
  (t460.adb:12#11)^t460.r =(int32) value_of!0!m + 96_int32;
}

{ { int32 32; int32 0; }64 64; { int32 32; int32 0; }64 0; }128 t460.m(int32 y) {
  (t460.adb:7#12)^!return =128 t460.mi + ((belongs[1,2] y_int32 - 1) * 128);
}

{ { int32 32; int32 0; }64 64; { int32 32; int32 0; }64 0; }128[2] t460.mi;
int32 t460.r;
int32 t460.x;
(t460.adb:3#6)^t460.x =(int32) 1;

