
L = {H=0093423b, U=4, trait = Signed I[-2147483648;2147483647], range = <unlimited>}
R = {H=000008d2, U=6, trait = Float 6, range = <unlimited>}
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

void t499(void) {
  (t499.adb:5#8)^int32 xix;
  (t499.adb:6#5)^float32 y;
  (t499.adb:5#8)^xix =(int32) 5;
  (t499.adb:8#18)^y: float32 <- t497a.inc_in3(xix_int32: int32);
}


