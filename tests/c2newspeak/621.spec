Warning: 621.c:30#0: goto statement accepted
Warning: 621.c:31#0: goto statement accepted
Newspeak output
---------------
void (621.c:26#5)^main(void) {
  (621.c:27#4)^uint32 break.621.c:29#13.0;
  (621.c:27#4)^break.621.c:29#13.0 =(uint32) 0;
  (621.c:27#4)^{
    uint32 break.621.c:29#13.1;
    (621.c:27#4)^break.621.c:29#13.1 =(uint32) 0;
    (621.c:27#4)^{
      uint32 goto!lbl;
      (621.c:27#4)^goto!lbl =(uint32) 0;
      (621.c:27#4)^do {
        (621.c:27#4)^while (1) {
          (621.c:27#13)^do {
            (621.c:27#13)^while (1) {
              (621.c:27#13)^break.621.c:29#13.1 =(uint32) 1;
              (621.c:29#13)^break.621.c:29#13.0 =(uint32) 1;
              (621.c:27#13)^choose {
               -->
                (621.c:27#13)^guard(break.621.c:29#13.0_int32);
                (621.c:27#13)^break.621.c:29#13.0 =(uint32) 0;
                (621.c:27#13)^goto lbl4;
               -->
                (621.c:27#13)^guard(! break.621.c:29#13.0_int32);
              }
            }
          } with lbl4:
          (621.c:27#13)^choose {
           -->
            (621.c:27#13)^guard(break.621.c:29#13.1_int32);
            (621.c:27#13)^break.621.c:29#13.1 =(uint32) 0;
            (621.c:27#13)^goto lbl1;
           -->
            (621.c:27#13)^guard(! break.621.c:29#13.1_int32);
          }
        }
      } with lbl1:
    }
  }
}


