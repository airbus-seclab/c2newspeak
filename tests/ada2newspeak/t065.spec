Newspeak Object output
----------------------
t065.adb
Global used

Global variables

Function definitions
t065() {
  do {
    uint2 ae;
    uint2 be;
    uint1 ab;
    uint1 bb;
    uint1 cb;
    4- =(uint2) belongs[0,3-1] 1;
    3- =(uint2) belongs[0,3-1] 4-_uint2;
    2- =(uint1) (4-_uint2 ==_uint2 2);
    1- =(uint1) (3-_uint2 ==_uint2 4-_uint2);
    {
      uint1 tmp0;
            choose {
       -->
        guard((6 > 4));
        0- =(uint1) 1;
       -->
        guard((4 > 6));
        {
          uint1 tmp1;
                    choose {
           -->
            guard((4. > 4.));
            0- =(uint1) (-57.51168 > 57.51168);
           -->
            guard((4. > 4.));
            0- =(uint1) 0;
          }
          1- =(uint1) 0-_uint1;
        }
      }
      1- =(uint1) 0-_uint1;
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t065.adb
void t065(void) {
  (t065.adb:25#611)^uint2 ae;
  (t065.adb:26#640)^uint2 be;
  (t065.adb:28#670)^uint1 ab;
  (t065.adb:29#707)^uint1 bb;
  (t065.adb:30#745)^uint1 cb;
  (t065.adb:25#611)^4- =(uint2) 1;
  (t065.adb:26#640)^3- =(uint2) belongs[0,2] 4-_uint2;
  (t065.adb:28#670)^2- =(uint1) (4-_uint2 ==_uint2 2);
  (t065.adb:29#707)^1- =(uint1) ! (4-_uint2 ==_uint2 3-_uint2);
  (t065.adb:30#745)^{
    uint1 tmp0;
    (t065.adb:30#745)^choose {
     -->
      (t065.adb:30#745)^guard((6 > 4));
      (t065.adb:30#745)^0- =(uint1) 1;
     -->
      (t065.adb:30#745)^guard(! (6 > 4));
      (t065.adb:30#745)^{
        uint1 tmp1;
        (t065.adb:30#745)^choose {
         -->
          (t065.adb:30#745)^guard(! (4. > 4.));
          (t065.adb:30#745)^0- =(uint1) ! (57.51168 > -57.51168);
         -->
          (t065.adb:30#745)^guard((4. > 4.));
          (t065.adb:30#745)^0- =(uint1) 0;
        }
        (t065.adb:30#745)^1- =(uint1) 0-_uint1;
      }
    }
    (t065.adb:30#745)^1- =(uint1) 0-_uint1;
  }
}


