Newspeak output
---------------
void t480(void) {
  (t480.adb:3#6)^int32 i;
  (t480.adb:5#7)^int32[3] value_of!0!g;
  (t480.adb:5#7)^value_of!0!g: int32[3] <- t480a.g();
  (t480.adb:5#7)^i =(int32) value_of!0!g + 32_int32;
}

int32[3] t480a.g(void) {
  (t480a.adb:5#12)^!return =3 t480a.res;
}

int32[3] t480a.res;

