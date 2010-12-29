Warning: 582.c:31#0: goto statement accepted
Newspeak output
---------------
void (582.c:26#5)^main(void) {
  (582.c:27#6)^uint32 goto!lbl;
  (582.c:27#6)^goto!lbl =(uint32) 0;
  (582.c:27#6)^{
    int32 i;
    (582.c:28#2)^do {
      (582.c:28#2)^while (1) {
        (582.c:28#2)^do {
          (582.c:28#2)^while (1) {
            (582.c:29#2)^goto!lbl =(uint32) 0;
            (582.c:29#7)^i =(int32) 0;
            (582.c:28#2)^choose {
             -->
              (582.c:28#2)^guard(! (i_int32 ==_int32 0));
             -->
              (582.c:28#2)^guard((i_int32 ==_int32 0));
              (582.c:28#2)^goto lbl3;
            }
          }
        } with lbl3:
        (582.c:28#2)^goto!lbl =(uint32) 1;
        (582.c:28#2)^choose {
         -->
          (582.c:28#2)^guard(goto!lbl_int32);
         -->
          (582.c:28#2)^guard(! goto!lbl_int32);
          (582.c:28#2)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


