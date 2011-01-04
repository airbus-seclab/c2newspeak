Newspeak output
---------------
void t425(void) {
  (t425.adb:7#9)^int32[3] data;
  (t425.adb:8#6)^int32 l;
  (t425.adb:8#6)^l =(int32) 6;
  (t425.adb:10#10)^{
    int32[3] tmp_cir!0;
    (t425.adb:10#10)^tmp_cir!0: int32[3] <- t425a.get_data(2: int32);
    (t425.adb:10#10)^data: int32[3] <- t425b.mul(tmp_cir!0_int32[3]: int32[3], l_int32: int32);
  }
}

int32[3] t425a.get_data(int32 x) {
  (t425a.adb:4#9)^int32[3] y;
  (t425a.adb:6#12)^y =(int32) 1;
  (t425a.adb:7#12)^y + 32 =(int32) x_int32;
  (t425a.adb:8#12)^y + 64 =(int32) 3;
  (t425a.adb:9#12)^!return =3 y;
}

int32[3] t425b.mul(int32[3] vector_left, int32 scalar_right) {
  (t425b.adb:6#14)^int32[3] result;
  (t425b.adb:6#14)^result =(int32) 7;
  (t425b.adb:6#14)^result + 32 =(int32) 7;
  (t425b.adb:6#14)^result + 64 =(int32) 7;
  (t425b.adb:8#12)^!return =3 result;
}


