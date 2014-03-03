
L = {H=00cb3550, U=4, trait = Signed I[-2147483648;2147483647], range = <unlimited>}
R = {H=33f64f6c, U=6, trait = Float 6, range = <unlimited>}
Newspeak output
---------------
void t497(void) {
  (t497.adb:4#6)^float32 x;
  (t497.adb:5#5)^int32 y;
  (t497.adb:4#6)^x =(float32) 3.2;
  (t497.adb:9#18)^x: float32 <- t497a.incr((int32 <= float32) x_float32: int32);
}

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


