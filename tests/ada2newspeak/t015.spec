Newspeak output
---------------
t015.adb
void t015(void) {
  (t015.adb:2#18)^int32 x;
  (t015.adb:4#38)^0- =(int32) 10;
  (t015.adb:5#48)^choose {
   -->
    (t015.adb:5#48)^guard((2 > 0-_int32));
    (t015.adb:7#67)^0- =(int32) belongs[-2147483648,2147483647] (0-_int32 + 1);
   -->
    (t015.adb:5#48)^guard(! (2 > 0-_int32));
    (t015.adb:9#89)^0- =(int32) belongs[-2147483648,2147483647] (0-_int32 - 1);
  }
}


