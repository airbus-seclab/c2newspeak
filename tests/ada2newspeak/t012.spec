Newspeak output
---------------
void t012(void) {
  (t012.adb:3#6)^int32 x;
  (t012.adb:3#6)^x =(int32) 3;
  (t012.adb:2#9)^do {
    (t012.adb:5#5)^choose {
     -->
      (t012.adb:5#5)^guard((x_int32 > 2));
      (t012.adb:7#10)^x =(int32) belongs[-2147483648,2147483647] (x_int32 + 1);
      (t012.adb:8#10)^x =(int32) belongs[-2147483648,2147483647] (x_int32 + 4);
      (t012.adb:9#8)^choose {
       -->
        (t012.adb:9#8)^guard((10 > x_int32));
        (t012.adb:11#13)^x =(int32) belongs[-2147483648,2147483647] (x_int32 * 2);
       -->
        (t012.adb:9#8)^guard(! (10 > x_int32));
        (t012.adb:12#11)^choose {
         -->
          (t012.adb:12#11)^guard((x_int32 ==_int32 42));
          (t012.adb:14#15)^goto lbl0;
         -->
          (t012.adb:12#11)^guard(! (x_int32 ==_int32 42));
          (t012.adb:15#11)^choose {
           -->
            (t012.adb:15#11)^guard((26 > x_int32));
            (t012.adb:17#13)^x =(int32) 42;
           -->
            (t012.adb:15#11)^guard(! (26 > x_int32));
            (t012.adb:19#13)^x =(int32) belongs[-2147483648,2147483647] (x_int32 - 1);
          }
        }
      }
     -->
      (t012.adb:5#5)^guard(! (x_int32 > 2));
    }
  } with lbl0:
}


