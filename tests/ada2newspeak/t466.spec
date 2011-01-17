Newspeak output
---------------
void t466(void) {
  (t466.adb:4#6)^uint2 r;
  (t466.adb:5#8)^int32 tmp;
  (t466.adb:7#7)^choose {
   -->
    (t466.adb:7#7)^guard((r_uint2 ==_uint2 0));
    (t466.adb:8#22)^tmp =(int32) 1;
   -->
    (t466.adb:7#7)^choose {
     -->
      (t466.adb:7#7)^guard((r_uint2 ==_uint2 1));
      (t466.adb:9#22)^tmp =(int32) 2;
     -->
      (t466.adb:7#7)^choose {
       -->
        (t466.adb:7#7)^guard((r_uint2 ==_uint2 2));
        (t466.adb:10#22)^tmp =(int32) 3;
       -->
        (t466.adb:7#7)^guard(! (r_uint2 ==_uint2 2));
        (t466.adb:7#7)^guard(! (r_uint2 ==_uint2 1));
        (t466.adb:7#7)^guard(! (r_uint2 ==_uint2 0));
      }
    }
  }
}


