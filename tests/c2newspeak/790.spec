Warning: 790.c:9#0: goto statement accepted
Newspeak output
---------------
790.c
void main(void) {
  (790.c:3#2)^uint32 goto!lbl;
  (790.c:3#2)^goto!lbl =(uint32) 0;
  (790.c:4#8)^{
    int32 i;
    (790.c:3#2)^while (1) {
      (790.c:5#4)^f(i_int32: int32);
      (790.c:3#2)^do {
        (790.c:6#4)^while (1) {
          (790.c:6#11)^choose {
           -->
            (790.c:6#11)^guard(! (goto!lbl_uint32 ==_uint32 0));
           -->
            (790.c:6#11)^guard((goto!lbl_uint32 ==_uint32 0));
          }
          (790.c:6#4)^goto!lbl =(uint32) 1;
          (790.c:6#4)^choose {
           -->
            (790.c:6#4)^guard(goto!lbl_int32);
           -->
            (790.c:6#4)^guard(! goto!lbl_int32);
            (790.c:6#4)^goto lbl3;
          }
        }
      } with lbl3:
    }
  }
}


