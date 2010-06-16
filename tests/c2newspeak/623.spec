Warning: 623.c:34#0: goto statement accepted
Newspeak output
---------------
623.c
void main(void) {
  (623.c:27#2)^uint32 goto!fail;
  (623.c:27#2)^goto!fail =(uint32) 0;
  (623.c:27#2)^while (1) {
    (623.c:27#2)^do {
      (623.c:33#4)^while (1) {
        (623.c:28#11)^choose {
         -->
          (623.c:28#11)^guard(! (goto!fail_uint32 ==_uint32 0));
          (623.c:29#12)^choose {
           -->
            (623.c:29#12)^guard(! (goto!fail_uint32 ==_uint32 0));
           -->
            (623.c:29#12)^guard((goto!fail_uint32 ==_uint32 0));
          }
         -->
          (623.c:28#11)^guard((goto!fail_uint32 ==_uint32 0));
        }
        (623.c:33#4)^goto!fail =(uint32) 1;
        (623.c:33#4)^choose {
         -->
          (623.c:33#4)^guard(goto!fail_int32);
         -->
          (623.c:33#4)^guard(! goto!fail_int32);
          (623.c:33#4)^goto lbl2;
        }
      }
    } with lbl2: {
    }
  }
}


