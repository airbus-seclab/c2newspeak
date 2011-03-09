Warning: 583.c:30#2: goto statement accepted
Newspeak output
---------------
void (583.c:26#5)^main(void) {
  (583.c:27#6)^uint32 goto!lbl;
  (583.c:27#6)^goto!lbl =(uint32) 0;
  (583.c:27#6)^{
    int32 i;
    (583.c:28#1)^do {
      (583.c:28#1)^while (1) {
        (583.c:29#2)^i =(int32) 1;
        (583.c:29#2)^do {
          (583.c:29#2)^while (1) {
            (583.c:30#2)^goto!lbl =(uint32) 1;
            (583.c:30#2)^choose {
             -->
              (583.c:30#2)^guard(goto!lbl_int32);
              (583.c:30#2)^goto lbl4;
             -->
              (583.c:30#2)^guard(! goto!lbl_int32);
            }
            (583.c:29#2)^choose {
             -->
              (583.c:29#2)^guard(! (i_int32 ==_int32 0));
             -->
              (583.c:29#2)^guard((i_int32 ==_int32 0));
              (583.c:29#2)^goto lbl4;
            }
          }
        } with lbl4:
        (583.c:28#1)^choose {
         -->
          (583.c:28#1)^guard(goto!lbl_int32);
         -->
          (583.c:28#1)^guard(! goto!lbl_int32);
          (583.c:28#1)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


