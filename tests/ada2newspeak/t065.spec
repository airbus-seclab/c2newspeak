Newspeak output
---------------
t065.adb
void t065(void) {
  (t065.adb:25#7)^uint2 ae;
  (t065.adb:26#7)^uint2 be;
  (t065.adb:28#7)^uint1 ab;
  (t065.adb:29#7)^uint1 bb;
  (t065.adb:30#7)^uint1 cb;
  (t065.adb:25#7)^ae =(uint2) 1;
  (t065.adb:26#7)^be =(uint2) belongs[0,2] ae_uint2;
  (t065.adb:28#7)^ab =(uint1) (ae_uint2 ==_uint2 2);
  (t065.adb:29#7)^bb =(uint1) ! (ae_uint2 ==_uint2 be_uint2);
  (t065.adb:30#7)^{
    uint1 tmp0;
    (t065.adb:30#7)^choose {
     -->
      (t065.adb:30#7)^guard((6 > 4));
      (t065.adb:30#7)^tmp0 =(uint1) 1;
     -->
      (t065.adb:30#7)^guard(! (6 > 4));
      (t065.adb:30#7)^{
        uint1 tmp1;
        (t065.adb:30#7)^choose {
         -->
          (t065.adb:30#7)^guard(! (4. > 4.));
          (t065.adb:30#7)^tmp1 =(uint1) ! (57.51168 > -57.51168);
         -->
          (t065.adb:30#7)^guard((4. > 4.));
          (t065.adb:30#7)^tmp1 =(uint1) 0;
        }
        (t065.adb:30#7)^tmp0 =(uint1) tmp1_uint1;
      }
    }
    (t065.adb:30#7)^cb =(uint1) tmp0_uint1;
  }
}


