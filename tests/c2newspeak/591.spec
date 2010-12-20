Warning: 591.c:32#0: goto statement accepted
Newspeak output
---------------
void main(void) {
  (591.c:27#6)^uint32 goto!lbl;
  (591.c:27#6)^goto!lbl =(uint32) 0;
  (591.c:27#6)^{
    int32 i;
    (591.c:28#1)^do {
      (591.c:28#1)^while (1) {
        (591.c:29#2)^i =(int32) 1;
        (591.c:29#2)^choose {
         -->
          (591.c:29#2)^guard(! (i_int32 ==_int32 0));
         -->
          (591.c:29#2)^guard((i_int32 ==_int32 0));
          (591.c:29#2)^goto!lbl =(uint32) 1;
          (591.c:32#4)^choose {
           -->
            (591.c:32#4)^guard(! goto!lbl_int32);
            (591.c:29#2)^i =(int32) 0;
           -->
            (591.c:32#4)^guard(goto!lbl_int32);
          }
        }
        (591.c:28#1)^choose {
         -->
          (591.c:28#1)^guard(goto!lbl_int32);
         -->
          (591.c:28#1)^guard(! goto!lbl_int32);
          (591.c:28#1)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


