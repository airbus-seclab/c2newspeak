Warning: 790.c:9#0: goto statement accepted
Newspeak output
---------------
790.c
void main(void) {
  (790.c:3#2)^uint32 goto.lbl;
  (790.c:3#2)^0- =(uint32) 0;
  (790.c:4#8)^{
    int32 i;
    (790.c:3#2)^while (1) {
      (790.c:5#4)^{
        int32 f.arg1;
        (790.c:5#4)^0- =(int32) 1-_int32;
        (790.c:5#4)^f();
      }
      (790.c:3#2)^do {
        (790.c:6#4)^while (1) {
          (790.c:6#11)^choose {
           -->
            (790.c:6#11)^guard(! (1-_uint32 ==_uint32 0));
           -->
            (790.c:6#11)^guard((1-_uint32 ==_uint32 0));
          }
          (790.c:6#4)^1- =(uint32) 1;
          (790.c:6#4)^choose {
           -->
            (790.c:6#4)^guard(1-_int32);
           -->
            (790.c:6#4)^guard(! 1-_int32);
            (790.c:6#4)^goto lbl3;
          }
        }
      } with lbl3: {
      }
    }
  }
}


