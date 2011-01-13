Newspeak output
---------------
void t462.a(void) {
  (t462.adb:12#18)^{ int32 32; int32 0; }64 value_of!0!m;
  (t462.adb:12#18)^value_of!0!m: { int32 32; int32 0; }64 <- t462.m(belongs[1,2] t462.x_int32: int32);
  (t462.adb:12#18)^value_of!0!m + 32 =(int32) t462.r_int32;
}

{ int32 32; int32 0; }64 t462.m(int32 y) {
  (t462.adb:8#12)^!return =64 t462.mi + ((belongs[1,2] y_int32 - 1) * 64);
}

{ int32 32; int32 0; }64[2] t462.mi;
int32 t462.r;
int32 t462.x;
(t462.adb:3#7)^t462.x =(int32) 1;

