Warning: 586.c:36#0: goto statement accepted
Newspeak output
---------------
586.c
void main(void) {
  (586.c:27#6)^uint32 goto!lbl;
  (586.c:27#6)^goto!lbl =(uint32) 0;
  (586.c:27#6)^{
    int32 i;
    (586.c:28#2)^uint32 switch.586.c:28#2.0;
    (586.c:28#2)^do {
      (586.c:28#2)^while (1) {
        (586.c:28#2)^choose {
         -->
          (586.c:28#2)^guard(! goto!lbl_int32);
          (586.c:28#2)^switch.586.c:28#2.0 =(uint32) coerce[0,4294967295] i_int32;
         -->
          (586.c:28#2)^guard(goto!lbl_int32);
          (586.c:28#2)^switch.586.c:28#2.0 =(uint32) 1;
        }
        (586.c:28#2)^do {
          (586.c:28#2)^do {
            (586.c:29#2)^do {
              (586.c:28#2)^choose {
               -->
                (586.c:28#2)^guard((switch.586.c:28#2.0_uint32 ==_int32 1));
                (586.c:29#2)^goto lbl5;
               -->
                (586.c:28#2)^guard(! (switch.586.c:28#2.0_uint32 ==_int32 1));
                (586.c:28#2)^goto lbl4;
              }
            } with lbl5:
            (586.c:30#7)^i =(int32) 0;
            (586.c:31#4)^goto lbl3;
          } with lbl4:
          (586.c:33#4)^i =(int32) 2;
        } with lbl3:
        (586.c:28#2)^goto!lbl =(uint32) 1;
        (586.c:28#2)^choose {
         -->
          (586.c:28#2)^guard(goto!lbl_int32);
         -->
          (586.c:28#2)^guard(! goto!lbl_int32);
          (586.c:28#2)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


