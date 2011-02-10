Newspeak output
---------------
void t481({ int32 160; { int32[3] 32; int32 0; }128 32; int32 0; }192 data) {
  (t481.adb:5#6)^int32 x;
  (t481.adb:6#10)^{ int32[3] 32; int32 0; }128 data2;
  (t481.adb:8#7)^int32 i;
  (t481.adb:8#7)^i =(int32) 1;
  (t481.adb:8#7)^do {
    (t481.adb:8#7)^while (1) {
      (t481.adb:8#7)^choose {
       -->
        (t481.adb:8#7)^guard(! (i_int32 > 3));
       -->
        (t481.adb:8#7)^guard((i_int32 > 3));
        (t481.adb:8#7)^goto lbl1;
      }
      (t481.adb:9#21)^data2 + 32 + ((belongs[1,3] i_int32 - 1) * 32) =(int32) data + 64 + ((belongs[1,3] i_int32 - 1) * 32)_int32;
      (t481.adb:8#7)^i =(int32) belongs[1,3] (i_int32 + 1);
    }
  } with lbl1:
}


