Newspeak output
---------------
void t015(void) {
  (t015.adb:2#5)^int32 x;
  (t015.adb:4#6)^x =(int32) 10;
  (t015.adb:5#5)^choose {
   -->
    (t015.adb:5#5)^guard((2 > x_int32));
    (t015.adb:7#9)^x =(int32) belongs[-2147483648,2147483647] (x_int32 + 1);
   -->
    (t015.adb:5#5)^guard(! (2 > x_int32));
    (t015.adb:9#9)^x =(int32) belongs[-2147483648,2147483647] (x_int32 - 1);
  }
}


