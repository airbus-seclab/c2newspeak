Warning: 603.c:30#0: goto statement accepted
Newspeak output
---------------
void (603.c:26#5)^main(void) {
  (603.c:27#6)^uint32 goto!lbl;
  (603.c:27#6)^goto!lbl =(uint32) 0;
  (603.c:27#6)^{
    int32 i;
    (603.c:28#2)^uint32 switch.603.c:28#2.0;
    (603.c:28#2)^do {
      (603.c:28#2)^while (1) {
        (603.c:28#2)^choose {
         -->
          (603.c:28#2)^guard(! goto!lbl_int32);
          (603.c:28#2)^switch.603.c:28#2.0 =(uint32) coerce[0,4294967295] i_int32;
         -->
          (603.c:28#2)^guard(goto!lbl_int32);
          (603.c:28#2)^switch.603.c:28#2.0 =(uint32) 1;
        }
        (603.c:28#2)^do {
          (603.c:28#2)^do {
            (603.c:29#2)^do {
              (603.c:28#2)^choose {
               -->
                (603.c:28#2)^guard((switch.603.c:28#2.0_uint32 ==_int32 0));
                (603.c:29#2)^goto lbl5;
               -->
                (603.c:28#2)^guard(! (switch.603.c:28#2.0_uint32 ==_int32 0));
                (603.c:28#2)^goto lbl4;
              }
            } with lbl5:
            (603.c:30#4)^goto!lbl =(uint32) 1;
            (603.c:30#4)^choose {
             -->
              (603.c:30#4)^guard(goto!lbl_int32);
              (603.c:30#4)^goto lbl3;
             -->
              (603.c:30#4)^guard(! goto!lbl_int32);
            }
          } with lbl4:
          (603.c:32#4)^while (1) {
            (603.c:33#4)^goto!lbl =(uint32) 0;
          }
        } with lbl3:
        (603.c:28#2)^choose {
         -->
          (603.c:28#2)^guard(goto!lbl_int32);
         -->
          (603.c:28#2)^guard(! goto!lbl_int32);
          (603.c:28#2)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


