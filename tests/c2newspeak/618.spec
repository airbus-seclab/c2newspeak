Warning: 618.c:32#0: goto statement accepted
Newspeak output
---------------
618.c
void main(void) {
  (618.c:27#4)^uint32 continue.618.c:31#13.0;
  (618.c:27#4)^continue.618.c:31#13.0 =(uint32) 0;
  (618.c:27#4)^{
    uint32 goto!lbl;
    (618.c:27#4)^goto!lbl =(uint32) 0;
    (618.c:27#4)^while (1) {
      (618.c:31#6)^do {
        (618.c:31#6)^while (1) {
          (618.c:28#6)^while (1) {
            (618.c:28#6)^choose {
             -->
              (618.c:28#6)^guard(! (goto!lbl_uint32 ==_uint32 0));
             -->
              (618.c:28#6)^guard((goto!lbl_uint32 ==_uint32 0));
            }
            (618.c:29#6)^goto!lbl =(uint32) 0;
          }
          (618.c:31#13)^continue.618.c:31#13.0 =(uint32) 1;
          (618.c:31#6)^do {
            (618.c:31#13)^goto lbl5;
            (618.c:31#6)^goto!lbl =(uint32) 1;
            (618.c:31#6)^choose {
             -->
              (618.c:31#6)^guard(goto!lbl_int32);
             -->
              (618.c:31#6)^guard(! goto!lbl_int32);
              (618.c:31#6)^goto lbl4;
            }
          } with lbl5:
        }
      } with lbl4:
      (618.c:27#4)^do {
        (618.c:28#6)^choose {
         -->
          (618.c:28#6)^guard(continue.618.c:31#13.0_int32);
          (618.c:28#6)^continue.618.c:31#13.0 =(uint32) 0;
          (618.c:28#6)^goto lbl3;
         -->
          (618.c:28#6)^guard(! continue.618.c:31#13.0_int32);
        }
      } with lbl3:
    }
  }
}


