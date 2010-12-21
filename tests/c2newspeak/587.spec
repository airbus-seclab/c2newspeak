Warning: 587.c:39#0: goto statement accepted
Newspeak output
---------------
void main(void) {
  (587.c:27#6)^uint32 goto!lbl;
  (587.c:27#6)^goto!lbl =(uint32) 0;
  (587.c:27#6)^{
    int32 i;
    (587.c:28#2)^uint32 switch.587.c:28#2.0;
    (587.c:28#2)^do {
      (587.c:28#2)^while (1) {
        (587.c:28#2)^choose {
         -->
          (587.c:28#2)^guard(! goto!lbl_int32);
          (587.c:28#2)^switch.587.c:28#2.0 =(uint32) coerce[0,4294967295] i_int32;
         -->
          (587.c:28#2)^guard(goto!lbl_int32);
          (587.c:28#2)^switch.587.c:28#2.0 =(uint32) 0;
        }
        (587.c:28#2)^do {
          (587.c:28#2)^do {
            (587.c:32#2)^do {
              (587.c:29#2)^do {
                (587.c:28#2)^choose {
                 -->
                  (587.c:28#2)^guard((switch.587.c:28#2.0_uint32 ==_int32 1));
                  (587.c:29#2)^goto lbl6;
                 -->
                  (587.c:28#2)^choose {
                   -->
                    (587.c:28#2)^guard((switch.587.c:28#2.0_uint32 ==_int32 2));
                    (587.c:32#2)^goto lbl5;
                   -->
                    (587.c:28#2)^guard(! (switch.587.c:28#2.0_uint32 ==_int32 2));
                    (587.c:28#2)^guard(! (switch.587.c:28#2.0_uint32 ==_int32 1));
                    (587.c:28#2)^goto lbl4;
                  }
                }
              } with lbl6:
              (587.c:30#4)^i =(int32) 2;
              (587.c:31#4)^goto lbl3;
            } with lbl5:
            (587.c:33#4)^i =(int32) 1;
            (587.c:34#4)^goto lbl3;
          } with lbl4:
          (587.c:36#7)^i =(int32) 0;
        } with lbl3:
        (587.c:28#2)^goto!lbl =(uint32) 1;
        (587.c:28#2)^choose {
         -->
          (587.c:28#2)^guard(goto!lbl_int32);
         -->
          (587.c:28#2)^guard(! goto!lbl_int32);
          (587.c:28#2)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


