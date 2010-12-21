Warning: t402.ads:2#18: multiple definitions of global variable t402.c_float_zero, in file t402.ads variable t402.c_float_zero should probably be extern accepted
Newspeak output
---------------
void t401(void) {
  (t401.adb:3#6)^int32 x;
  (t401.adb:4#6)^uint1 b;
  (t401.adb:6#7)^b =(uint1) (t402.c_float_zero_int32 > x_int32);
}

int32 t403.s_get_int(float32 x) {
  (t403.adb:6#12)^!return =(int32) 0;
}

int32 t402.c_float_zero;

