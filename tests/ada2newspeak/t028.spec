Newspeak output
---------------
t028.adb
void t028(void) {
  (t028.adb:2#6)^int32 x;
  (t028.adb:2#6)^x =(int32) 0;
  (t028.adb:4#7)^while (1) {
    (t028.adb:5#8)^choose {
     -->
      (t028.adb:5#8)^guard((100 > x_int32));
      (t028.adb:6#13)^x =(int32) belongs[-2147483648,2147483647] (x_int32 + 1);
     -->
      (t028.adb:5#8)^guard(! (100 > x_int32));
      (t028.adb:8#13)^x =(int32) belongs[-2147483648,2147483647] (x_int32 - 100);
    }
  }
}


