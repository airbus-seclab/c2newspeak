
L = {H=2e06a9f9, U=8, trait = Signed I[-128;127], range = <unlimited>}
R = {H=00cb3550, U=4, trait = Signed I[-2147483648;2147483647], range = <unlimited>}
Newspeak output
---------------
void t487(void) {
  (t487.adb:15#7)^{ { int32[2][2] 32; int32 0; }160 32; int32 0; }192 a;
  (t487.adb:16#7)^int8 r;
  (t487.adb:18#7)^r =(int8) belongs[-128,127] a + 64_int32;
}


