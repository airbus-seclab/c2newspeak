Warning: 615.c:30#0: goto statement accepted
Newspeak output
---------------
void main(void) {
  (615.c:27#4)^uint32 break.615.c:29#13.0;
  (615.c:27#4)^break.615.c:29#13.0 =(uint32) 0;
  (615.c:27#4)^{
    uint32 goto!lbl;
    (615.c:27#4)^goto!lbl =(uint32) 0;
    (615.c:27#4)^do {
      (615.c:27#4)^while (1) {
        (615.c:29#13)^break.615.c:29#13.0 =(uint32) 1;
        (615.c:27#13)^choose {
         -->
          (615.c:27#13)^guard(break.615.c:29#13.0_int32);
          (615.c:27#13)^break.615.c:29#13.0 =(uint32) 0;
          (615.c:27#13)^goto lbl1;
         -->
          (615.c:27#13)^guard(! break.615.c:29#13.0_int32);
        }
      }
    } with lbl1:
  }
}


