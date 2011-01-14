Newspeak output
---------------
void t465(void) {
  (t465.adb:3#8)^int17 c_m;
  (t465.adb:12#7)^int17 c;
  (t465.adb:13#10)^int17 ms;
  (t465.adb:15#7)^{ int32 32; int32 0; }64[32] m;
  (t465.adb:21#6)^int32[2] a;
  (t465.adb:3#8)^c_m =(int17) 32;
  (t465.adb:12#7)^c =(int17) 24;
  (t465.adb:13#10)^ms =(int17) 0;
  (t465.adb:23#6)^{
    int17 i;
    (t465.adb:23#6)^i =(int17) 1;
    (t465.adb:23#6)^do {
      (t465.adb:23#6)^while (1) {
        (t465.adb:23#6)^choose {
         -->
          (t465.adb:23#6)^guard(! (i_int17 > 2));
         -->
          (t465.adb:23#6)^guard((i_int17 > 2));
          (t465.adb:23#6)^goto lbl1;
        }
        (t465.adb:24#8)^choose {
         -->
          (t465.adb:24#8)^guard((2 > i_int17));
         -->
          (t465.adb:24#8)^guard(! (2 > i_int17));
        }
        (t465.adb:23#6)^i =(int32) belongs[1,2] (i_int17 + 1);
      }
    } with lbl1:
  }
}


