Warning: 615.c:30#1103: goto statement accepted
Newspeak output
---------------
615.c
void main(void) {
  (615.c:27#4)^uint32 break.615.c:29#13.0;
  (615.c:27#4)^0- =(uint32) 0;
  (615.c:27#4)^{
    uint32 goto.lbl;
    (615.c:27#4)^0- =(uint32) 0;
    (615.c:27#4)^do {
      (615.c:27#4)^while (1) {
        (615.c:27#13)^do {
          (615.c:27#13)^while (1) {
            (615.c:29#13)^1- =(uint32) 1;
            (615.c:29#13)^goto lbl4;
          }
        } with lbl4: {
        }
        (615.c:27#13)^choose {
         -->
          (615.c:27#13)^guard(1-_int32);
          (615.c:27#13)^1- =(uint32) 0;
          (615.c:27#13)^goto lbl1;
         -->
          (615.c:27#13)^guard(! 1-_int32);
        }
      }
    } with lbl1: {
    }
  }
}


