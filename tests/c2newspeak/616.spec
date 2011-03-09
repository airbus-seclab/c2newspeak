Warning: 616.c:32#6: goto statement accepted
Newspeak output
---------------
void (616.c:26#5)^main(void) {
  (616.c:27#4)^uint32 break.616.c:31#13.0;
  (616.c:27#4)^break.616.c:31#13.0 =(uint32) 0;
  (616.c:27#4)^{
    uint32 goto!lbl;
    (616.c:27#4)^goto!lbl =(uint32) 0;
    (616.c:27#4)^do {
      (616.c:27#4)^while (1) {
        (616.c:31#6)^do {
          (616.c:31#6)^while (1) {
            (616.c:28#6)^while (1) {
              (616.c:28#6)^choose {
               -->
                (616.c:28#6)^guard(! (goto!lbl_uint32 ==_uint32 0));
               -->
                (616.c:28#6)^guard((goto!lbl_uint32 ==_uint32 0));
              }
              (616.c:29#6)^goto!lbl =(uint32) 0;
            }
            (616.c:31#13)^break.616.c:31#13.0 =(uint32) 1;
            (616.c:31#13)^goto lbl4;
            (616.c:31#6)^goto!lbl =(uint32) 1;
            (616.c:31#6)^choose {
             -->
              (616.c:31#6)^guard(goto!lbl_int32);
             -->
              (616.c:31#6)^guard(! goto!lbl_int32);
              (616.c:31#6)^goto lbl4;
            }
          }
        } with lbl4:
        (616.c:28#6)^choose {
         -->
          (616.c:28#6)^guard(break.616.c:31#13.0_int32);
          (616.c:28#6)^break.616.c:31#13.0 =(uint32) 0;
          (616.c:28#6)^goto lbl1;
         -->
          (616.c:28#6)^guard(! break.616.c:31#13.0_int32);
        }
      }
    } with lbl1:
  }
}


