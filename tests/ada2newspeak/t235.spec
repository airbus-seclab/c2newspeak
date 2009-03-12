Newspeak output
---------------
t235.adb
void t235(void) {
  (t235.adb:8#183)^int32 x;
  (t235.adb:10#206)^int32 i;
  (t235.adb:10#206)^0- =(int32) 5;
  (t235.adb:10#206)^do {
    (t235.adb:10#206)^while (1) {
      (t235.adb:10#206)^choose {
       -->
        (t235.adb:10#206)^guard((0-_int32 > 10));
        (t235.adb:10#206)^goto lbl1;
       -->
        (t235.adb:10#206)^guard(! (0-_int32 > 10));
      }
      (t235.adb:11#230)^1- =(int32) 1;
      (t235.adb:10#206)^0- =(int32) belongs[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl1: {
  }
}


