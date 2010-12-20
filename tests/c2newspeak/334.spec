Warning: 334.c:30#0: goto statement accepted
Newspeak output
---------------
void main(void) {
  (334.c:27#6)^uint32 goto!lbl;
  (334.c:27#6)^goto!lbl =(uint32) 0;
  (334.c:27#6)^{
    int32 x;
    (334.c:28#1)^do {
      (334.c:28#1)^while (1) {
        (334.c:29#2)^do {
          (334.c:29#2)^while (1) {
            (334.c:29#2)^choose {
             -->
              (334.c:29#2)^guard(! (x_int32 ==_int32 0));
             -->
              (334.c:29#2)^guard((x_int32 ==_int32 0));
              (334.c:29#2)^goto lbl4;
            }
            (334.c:30#4)^goto!lbl =(uint32) 1;
            (334.c:30#4)^choose {
             -->
              (334.c:30#4)^guard(goto!lbl_int32);
              (334.c:30#4)^goto lbl4;
             -->
              (334.c:30#4)^guard(! goto!lbl_int32);
            }
            (334.c:31#4)^x =(int32) 0;
          }
        } with lbl4:
        (334.c:28#1)^choose {
         -->
          (334.c:28#1)^guard(goto!lbl_int32);
         -->
          (334.c:28#1)^guard(! goto!lbl_int32);
          (334.c:28#1)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


