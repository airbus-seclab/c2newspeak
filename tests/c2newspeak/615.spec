Warning: 615.c:30#1103: goto statement accepted
Newspeak output
---------------
615.c
void main(void) {
  (615.c:27#4)^uint32 goto.lbl;
  (615.c:27#4)^0- =(uint32) 0;
  (615.c:28#4)^{
    uint32 break.615.c:29#13;
    (615.c:27#4)^do {
      (615.c:27#4)^while (1) {
        (615.c:28#4)^0- =(uint32) 0;
        (615.c:28#4)^do {
          (615.c:29#13)^0- =(uint32) 1;
          (615.c:29#13)^goto lbl4;
          (615.c:28#4)^while (1) {
            (615.c:29#13)^0- =(uint32) 1;
            (615.c:29#13)^goto lbl4;
          }
        } with lbl4: {
        }
        (615.c:28#4)^choose {
         -->
          (615.c:28#4)^guard(0-_uint32);
          (615.c:28#4)^0- =(uint32) 0;
          (615.c:28#4)^goto lbl1;
         -->
          (615.c:28#4)^guard(! 0-_uint32);
        }
      }
    } with lbl1: {
    }
  }
}


