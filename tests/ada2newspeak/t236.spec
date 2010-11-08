Newspeak output
---------------
t236.adb
void t236(void) {
  (t236.adb:8#7)^int32 x;
  (t236.adb:10#7)^int32 i;
  (t236.adb:10#7)^i =(int32) 5;
  (t236.adb:10#7)^do {
    (t236.adb:10#7)^while (1) {
      (t236.adb:10#7)^choose {
       -->
        (t236.adb:10#7)^guard(! (i_int32 > 10));
       -->
        (t236.adb:10#7)^guard((i_int32 > 10));
        (t236.adb:10#7)^goto lbl1;
      }
      (t236.adb:11#12)^x =(int32) i_int32;
      (t236.adb:10#7)^i =(int32) belongs[-2147483648,2147483647] (i_int32 + 1);
    }
  } with lbl1:
}


