Warning: 655.c:30#0: goto statement accepted
Warning: 655.c:33#0: goto statement accepted
Warning: 655.c:35#0: goto statement accepted
Warning: 655.c:36#0: goto statement accepted
Newspeak output
---------------
void (655.c:25#5)^main(void) {
  (655.c:26#6)^uint32 goto!lbl1;
  (655.c:26#6)^goto!lbl1 =(uint32) 0;
  (655.c:26#6)^{
    uint32 goto!lbl2;
    (655.c:26#6)^goto!lbl2 =(uint32) 0;
    (655.c:26#6)^{
      int32 i;
      (655.c:27#2)^do {
        (655.c:27#2)^do {
          (655.c:28#2)^do {
            (655.c:27#2)^choose {
             -->
              (655.c:27#2)^guard((i_int32 ==_int32 2));
              (655.c:28#2)^goto lbl4;
             -->
              (655.c:27#2)^guard(! (i_int32 ==_int32 2));
              (655.c:27#2)^goto lbl3;
            }
          } with lbl4:
          (655.c:30#6)^goto!lbl1 =(uint32) 1;
          (655.c:30#6)^choose {
           -->
            (655.c:30#6)^guard(goto!lbl1_int32);
            (655.c:30#6)^goto lbl2;
           -->
            (655.c:30#6)^guard(! goto!lbl1_int32);
          }
        } with lbl3:
        (655.c:33#4)^goto!lbl1 =(uint32) 1;
        (655.c:33#4)^choose {
         -->
          (655.c:33#4)^guard(goto!lbl1_int32);
          (655.c:33#4)^goto lbl2;
         -->
          (655.c:33#4)^guard(! goto!lbl1_int32);
        }
      } with lbl2:
      (655.c:30#6)^choose {
       -->
        (655.c:30#6)^guard(! goto!lbl1_int32);
        (655.c:33#4)^choose {
         -->
          (655.c:33#4)^guard(! goto!lbl1_int32);
          (655.c:36#2)^goto!lbl2 =(uint32) 1;
          (655.c:37#1)^choose {
           -->
            (655.c:37#1)^guard(! (goto!lbl2_uint32 ==_uint32 0));
           -->
            (655.c:37#1)^guard((goto!lbl2_uint32 ==_uint32 0));
          }
         -->
          (655.c:33#4)^guard(goto!lbl1_int32);
        }
       -->
        (655.c:30#6)^guard(goto!lbl1_int32);
      }
    }
  }
}


