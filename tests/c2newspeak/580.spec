Warning: 580.c:30#0: goto statement accepted
Newspeak output
---------------
580.c
void main(void) {
  (580.c:27#6)^uint32 goto.lbl;
  (580.c:27#6)^0- =(uint32) 0;
  (580.c:27#6)^{
    int32 i;
    (580.c:28#1)^do {
      (580.c:28#1)^while (1) {
        (580.c:29#2)^0- =(int32) 1;
        (580.c:29#2)^do {
          (580.c:29#2)^while (1) {
            (580.c:29#2)^choose {
             -->
              (580.c:29#2)^guard(! (0-_int32 ==_int32 0));
             -->
              (580.c:29#2)^guard((0-_int32 ==_int32 0));
              (580.c:29#2)^goto lbl4;
            }
            (580.c:30#2)^1- =(uint32) 1;
            (580.c:30#2)^choose {
             -->
              (580.c:30#2)^guard(1-_int32);
              (580.c:30#2)^goto lbl4;
             -->
              (580.c:30#2)^guard(! 1-_int32);
            }
          }
        } with lbl4: {
        }
        (580.c:28#1)^choose {
         -->
          (580.c:28#1)^guard(1-_int32);
         -->
          (580.c:28#1)^guard(! 1-_int32);
          (580.c:28#1)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


