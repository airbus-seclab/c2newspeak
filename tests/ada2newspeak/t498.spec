
L = {H=0093423b, U=4, trait = Signed I[-2147483648;2147483647], range = <unlimited>}
R = {H=000008d2, U=6, trait = Float 6, range = <unlimited>}
Warning: t498.adb:7#20: Must be cast back be a left-value or a cast, remove warning in t369
Warning: t498.adb:7#20: Must be cast back be a left-value or a cast, remove warning in t369
Newspeak output
---------------
void t497a.inc_in(int32 x, int32 y) {
  (t497a.adb:4#10)^y =(int32) belongs[-2147483648,2147483647] (x_int32 + 1);
}

void t497a.inc_in2(int32 x, int32 y, int32 z) {
  (t497a.adb:8#10)^y =(int32) belongs[-2147483648,2147483647] (x_int32 + 1);
  (t497a.adb:9#10)^z =(int32) belongs[-2147483648,2147483647] (x_int32 + 1);
}

void t497a.inc_in3(int32 x, int32 y) {
  (t497a.adb:14#10)^y =(int32) belongs[-2147483648,2147483647] (x_int32 + 1);
}

void t497a.incr(int32 x) {
  (t497a.adb:19#10)^x =(int32) x_int32;
}

void t498(void) {
  (t498.adb:3#8)^float32 xix;
  (t498.adb:4#5)^int32 y;
  (t498.adb:5#5)^int32 z;
  (t498.adb:3#8)^xix =(float32) 3.2;
  (t498.adb:7#20)^(y: int32, z: int32) <- t497a.inc_in2(belongs[-2147483648,2147483647] (int32 <= float32) xix_float32: int32, y_int32: int32, z_int32: int32);
}


