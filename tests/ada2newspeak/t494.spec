Warning: t494.adb:7#12: Must be cast back be a left-value or a cast, remove warning in t369
Newspeak output
---------------
void t494(void) {
  (t494.adb:3#7)^int32 fl;
  (t494.adb:5#8)^fl =(int32) 7;
  (t494.adb:7#12)^fl: int32 <- t494a.f(fl_int32: int32, 3: int32);
}

void t494a.f(int32 x, int32 xx) {
  (t494a.adb:4#10)^x =(int32) belongs[-2147483648,2147483647] (x_int32 + xx_int32);
}

void t494a.g(int32 x) {
}


