Warning: 579.c:31#2: goto statement accepted
Newspeak output
---------------
void (579.c:26#5)^main(void) {
  (579.c:27#6)^uint32 goto!lbl;
  (579.c:27#6)^goto!lbl =(uint32) 0;
  (579.c:27#6)^{
    int32 i;
    (579.c:28#2)^do {
      (579.c:28#2)^while (1) {
        (579.c:28#2)^do {
          (579.c:28#2)^while (1) {
            (579.c:28#2)^choose {
             -->
              (579.c:28#2)^choose {
               -->
                (579.c:28#2)^guard(! (goto!lbl_uint32 ==_uint32 0));
               -->
                (579.c:28#2)^guard((goto!lbl_uint32 ==_uint32 0));
                (579.c:28#2)^guard(! (i_int32 ==_int32 0));
              }
             -->
              (579.c:28#2)^guard((goto!lbl_uint32 ==_uint32 0));
              (579.c:28#2)^guard((i_int32 ==_int32 0));
              (579.c:28#2)^goto lbl3;
            }
            (579.c:29#2)^goto!lbl =(uint32) 0;
            (579.c:29#7)^i =(int32) 0;
          }
        } with lbl3:
        (579.c:28#2)^goto!lbl =(uint32) 1;
        (579.c:28#2)^choose {
         -->
          (579.c:28#2)^guard(goto!lbl_int32);
         -->
          (579.c:28#2)^guard(! goto!lbl_int32);
          (579.c:28#2)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


