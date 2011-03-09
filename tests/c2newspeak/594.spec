Warning: 594.c:32#4: goto statement accepted
Newspeak output
---------------
void (594.c:26#5)^main(void) {
  (594.c:27#6)^uint32 goto!lbl;
  (594.c:27#6)^goto!lbl =(uint32) 0;
  (594.c:27#6)^{
    int32 i;
    (594.c:31#2)^do {
      (594.c:31#2)^while (1) {
        (594.c:28#2)^do {
          (594.c:28#2)^while (1) {
            (594.c:28#2)^choose {
             -->
              (594.c:28#2)^choose {
               -->
                (594.c:28#2)^guard(! (goto!lbl_uint32 ==_uint32 0));
               -->
                (594.c:28#2)^guard((goto!lbl_uint32 ==_uint32 0));
                (594.c:28#2)^guard(! (i_int32 ==_int32 0));
              }
             -->
              (594.c:28#2)^guard((goto!lbl_uint32 ==_uint32 0));
              (594.c:28#2)^guard((i_int32 ==_int32 0));
              (594.c:28#2)^goto lbl3;
            }
            (594.c:29#2)^goto!lbl =(uint32) 0;
            (594.c:29#7)^i =(int32) 0;
          }
        } with lbl3:
        (594.c:31#2)^choose {
         -->
          (594.c:31#2)^guard(! (i_int32 ==_int32 0));
          (594.c:31#2)^goto!lbl =(uint32) 1;
         -->
          (594.c:31#2)^guard((i_int32 ==_int32 0));
        }
        (594.c:31#2)^choose {
         -->
          (594.c:31#2)^guard(goto!lbl_int32);
         -->
          (594.c:31#2)^guard(! goto!lbl_int32);
          (594.c:31#2)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


