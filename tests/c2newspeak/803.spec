Warning: 803.c:3#0: goto statement accepted
Warning: 803.c:4#0: goto statement accepted
Newspeak output
---------------
803.c
void main(void) {
  (803.c:2#1)^uint32 goto.b;
  (803.c:2#1)^goto.b =(uint32) 0;
  (803.c:2#1)^{
    uint32 goto.aa;
    (803.c:2#1)^goto.aa =(uint32) 0;
    (803.c:5#1)^do {
      (803.c:2#1)^while (1) {
        (803.c:3#9)^goto.b =(uint32) 1;
        (803.c:3#9)^choose {
         -->
          (803.c:3#9)^guard(! goto.b_int32);
          (803.c:4#2)^goto.aa =(uint32) 1;
         -->
          (803.c:3#9)^guard(goto.b_int32);
        }
        (803.c:2#1)^choose {
         -->
          (803.c:2#1)^guard(goto.aa_int32);
         -->
          (803.c:2#1)^guard(! goto.aa_int32);
          (803.c:2#1)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


