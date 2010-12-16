Warning: 821.c:35#0: goto statement accepted
Newspeak output
---------------
821.c
void main(void) {
  (821.c:27#6)^uint32 goto!lbl;
  (821.c:27#6)^goto!lbl =(uint32) 0;
  (821.c:27#6)^{
    int32 i;
    (821.c:28#2)^uint32 switch.821.c:28#2.0;
    (821.c:28#2)^do {
      (821.c:28#2)^while (1) {
        (821.c:28#2)^choose {
         -->
          (821.c:28#2)^guard(! goto!lbl_int32);
          (821.c:28#2)^switch.821.c:28#2.0 =(uint32) coerce[0,4294967295] i_int32;
         -->
          (821.c:28#2)^guard(goto!lbl_int32);
          (821.c:28#2)^switch.821.c:28#2.0 =(uint32) 0;
        }
        (821.c:28#2)^do {
          (821.c:28#2)^do {
            (821.c:29#2)^do {
              (821.c:28#2)^choose {
               -->
                (821.c:28#2)^guard((switch.821.c:28#2.0_uint32 ==_int32 1));
                (821.c:29#2)^goto lbl5;
               -->
                (821.c:28#2)^guard(! (switch.821.c:28#2.0_uint32 ==_int32 1));
                (821.c:28#2)^goto lbl4;
              }
            } with lbl5:
            (821.c:30#4)^goto lbl3;
          } with lbl4:
        } with lbl3:
        (821.c:28#2)^goto!lbl =(uint32) 1;
        (821.c:28#2)^choose {
         -->
          (821.c:28#2)^guard(goto!lbl_int32);
         -->
          (821.c:28#2)^guard(! goto!lbl_int32);
          (821.c:28#2)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


