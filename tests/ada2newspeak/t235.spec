Newspeak output
---------------
t235.adb
void t235(void) {
  (t235.adb:8#7)^int32 x;
  (t235.adb:10#7)^int32 i;
  (t235.adb:10#7)^i =(int32) 5;
  (t235.adb:10#7)^do {
    (t235.adb:10#7)^while (1) {
      (t235.adb:10#7)^choose {
       -->
        (t235.adb:10#7)^guard(! (i_int32 > 10));
       -->
        (t235.adb:10#7)^guard((i_int32 > 10));
        (t235.adb:10#7)^goto lbl1;
      }
      (t235.adb:11#12)^x =(int32) 1;
      (t235.adb:10#7)^i =(int32) belongs[-2147483648,2147483647] (i_int32 + 1);
    }
  } with lbl1:
}


