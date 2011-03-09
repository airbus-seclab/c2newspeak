Warning: 592.c:31#4: goto statement accepted
Newspeak output
---------------
void (592.c:26#5)^main(void) {
  (592.c:27#6)^uint32 goto!lbl;
  (592.c:27#6)^goto!lbl =(uint32) 0;
  (592.c:27#6)^{
    int32 i;
    (592.c:28#1)^do {
      (592.c:28#1)^while (1) {
        (592.c:29#2)^i =(int32) 1;
        (592.c:29#2)^do {
          (592.c:30#2)^do {
            (592.c:29#2)^choose {
             -->
              (592.c:29#2)^guard((i_int32 ==_int32 1));
              (592.c:30#2)^goto lbl5;
             -->
              (592.c:29#2)^guard(! (i_int32 ==_int32 1));
              (592.c:29#2)^goto lbl4;
            }
          } with lbl5:
          (592.c:31#4)^goto!lbl =(uint32) 1;
          (592.c:31#4)^choose {
           -->
            (592.c:31#4)^guard(goto!lbl_int32);
            (592.c:31#4)^goto lbl4;
           -->
            (592.c:31#4)^guard(! goto!lbl_int32);
          }
        } with lbl4:
        (592.c:28#1)^choose {
         -->
          (592.c:28#1)^guard(goto!lbl_int32);
         -->
          (592.c:28#1)^guard(! goto!lbl_int32);
          (592.c:28#1)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


