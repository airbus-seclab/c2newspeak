Newspeak output
---------------
t012.adb
void t012(void) {
  (t012.adb:3#6)^int32 x;
  (t012.adb:3#6)^0- =(int32) 3;
  (t012.adb:2#9)^do {
    (t012.adb:5#5)^choose {
     -->
      (t012.adb:5#5)^guard((0-_int32 > 2));
      (t012.adb:7#10)^0- =(int32) belongs[-2147483648,2147483647] (0-_int32 + 1);
      (t012.adb:8#10)^0- =(int32) belongs[-2147483648,2147483647] (0-_int32 + 4);
      (t012.adb:9#8)^choose {
       -->
        (t012.adb:9#8)^guard((10 > 0-_int32));
        (t012.adb:11#13)^0- =(int32) belongs[-2147483648,2147483647] (0-_int32 * 2);
       -->
        (t012.adb:9#8)^guard(! (10 > 0-_int32));
        (t012.adb:12#11)^choose {
         -->
          (t012.adb:12#11)^guard((0-_int32 ==_int32 42));
          (t012.adb:14#15)^goto lbl0;
         -->
          (t012.adb:12#11)^guard(! (0-_int32 ==_int32 42));
          (t012.adb:15#11)^choose {
           -->
            (t012.adb:15#11)^guard((26 > 0-_int32));
            (t012.adb:17#13)^0- =(int32) 42;
           -->
            (t012.adb:15#11)^guard(! (26 > 0-_int32));
            (t012.adb:19#13)^0- =(int32) belongs[-2147483648,2147483647] (0-_int32 - 1);
          }
        }
      }
     -->
      (t012.adb:5#5)^guard(! (0-_int32 > 2));
    }
  } with lbl0: {
  }
}


