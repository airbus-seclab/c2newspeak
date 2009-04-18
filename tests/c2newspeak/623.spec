Warning: 623.c:34#1133: goto statement accepted
Newspeak output
---------------
623.c
void main(void) {
  (623.c:27#2)^uint32 goto.fail;
  (623.c:27#2)^0- =(uint32) 0;
  (623.c:27#2)^while (1) {
    (623.c:27#2)^do {
      (623.c:33#4)^while (1) {
        (623.c:28#11)^choose {
         -->
          (623.c:28#11)^guard(0-_uint32);
          (623.c:29#12)^choose {
           -->
            (623.c:29#12)^guard(0-_uint32);
           -->
            (623.c:29#12)^guard(! 0-_uint32);
          }
         -->
          (623.c:28#11)^guard(! 0-_uint32);
        }
        (623.c:33#4)^0- =(uint32) 1;
        (623.c:33#4)^choose {
         -->
          (623.c:33#4)^guard(0-_uint32);
         -->
          (623.c:33#4)^guard(! 0-_uint32);
          (623.c:33#4)^goto lbl2;
        }
      }
    } with lbl2: {
    }
  }
}


