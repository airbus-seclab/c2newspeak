Newspeak output
---------------
t250.adb
uint2 t250.!op+(uint2 left, uint2 right) {
  (t250.adb:9#10)^do {
    (t250.adb:11#8)^choose {
     -->
      (t250.adb:11#8)^guard((left_uint2 ==_uint2 0));
      (t250.adb:12#20)^choose {
       -->
        (t250.adb:12#20)^guard((right_uint2 ==_uint2 0));
        (t250.adb:12#46)^!return =(uint2) 1;
        (t250.adb:12#46)^goto lbl0;
       -->
        (t250.adb:12#20)^choose {
         -->
          (t250.adb:12#20)^guard((right_uint2 ==_uint2 1));
          (t250.adb:13#46)^!return =(uint2) 2;
          (t250.adb:13#46)^goto lbl0;
         -->
          (t250.adb:12#20)^choose {
           -->
            (t250.adb:12#20)^guard((right_uint2 ==_uint2 2));
            (t250.adb:14#46)^!return =(uint2) 0;
            (t250.adb:14#46)^goto lbl0;
           -->
            (t250.adb:12#20)^guard(! (right_uint2 ==_uint2 2));
            (t250.adb:12#20)^guard(! (right_uint2 ==_uint2 1));
            (t250.adb:12#20)^guard(! (right_uint2 ==_uint2 0));
          }
        }
      }
     -->
      (t250.adb:11#8)^choose {
       -->
        (t250.adb:11#8)^guard((left_uint2 ==_uint2 1));
        (t250.adb:17#20)^choose {
         -->
          (t250.adb:17#20)^guard((right_uint2 ==_uint2 0));
          (t250.adb:17#46)^!return =(uint2) 2;
          (t250.adb:17#46)^goto lbl0;
         -->
          (t250.adb:17#20)^choose {
           -->
            (t250.adb:17#20)^guard((right_uint2 ==_uint2 1));
            (t250.adb:18#46)^!return =(uint2) 0;
            (t250.adb:18#46)^goto lbl0;
           -->
            (t250.adb:17#20)^choose {
             -->
              (t250.adb:17#20)^guard((right_uint2 ==_uint2 2));
              (t250.adb:19#46)^!return =(uint2) 1;
              (t250.adb:19#46)^goto lbl0;
             -->
              (t250.adb:17#20)^guard(! (right_uint2 ==_uint2 2));
              (t250.adb:17#20)^guard(! (right_uint2 ==_uint2 1));
              (t250.adb:17#20)^guard(! (right_uint2 ==_uint2 0));
            }
          }
        }
       -->
        (t250.adb:11#8)^choose {
         -->
          (t250.adb:11#8)^guard((left_uint2 ==_uint2 2));
          (t250.adb:22#20)^choose {
           -->
            (t250.adb:22#20)^guard((right_uint2 ==_uint2 0));
            (t250.adb:22#46)^!return =(uint2) 0;
            (t250.adb:22#46)^goto lbl0;
           -->
            (t250.adb:22#20)^choose {
             -->
              (t250.adb:22#20)^guard((right_uint2 ==_uint2 1));
              (t250.adb:23#46)^!return =(uint2) 1;
              (t250.adb:23#46)^goto lbl0;
             -->
              (t250.adb:22#20)^choose {
               -->
                (t250.adb:22#20)^guard((right_uint2 ==_uint2 2));
                (t250.adb:24#46)^!return =(uint2) 2;
                (t250.adb:24#46)^goto lbl0;
               -->
                (t250.adb:22#20)^guard(! (right_uint2 ==_uint2 2));
                (t250.adb:22#20)^guard(! (right_uint2 ==_uint2 1));
                (t250.adb:22#20)^guard(! (right_uint2 ==_uint2 0));
              }
            }
          }
         -->
          (t250.adb:11#8)^guard(! (left_uint2 ==_uint2 2));
          (t250.adb:11#8)^guard(! (left_uint2 ==_uint2 1));
          (t250.adb:11#8)^guard(! (left_uint2 ==_uint2 0));
        }
      }
    }
  } with lbl0:
}

void t250.f(void) {
  (t250.adb:31#10)^uint2 x;
  (t250.adb:31#10)^uint2 y;
  (t250.adb:32#7)^uint2 z;
  (t250.adb:31#10)^x =(uint2) 0;
  (t250.adb:31#10)^y =(uint2) 0;
  (t250.adb:34#8)^{
    uint2 tmp_cir!0;
    (t250.adb:34#8)^tmp_cir!0 <- t250.!op+(belongs[0,2] x_uint2, belongs[0,2] y_uint2);
    (t250.adb:34#8)^z =(uint2) belongs[0,2] tmp_cir!0_uint2;
  }
}


