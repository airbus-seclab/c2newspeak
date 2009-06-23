Warning: 654.c:27#1061: goto statement accepted
Warning: 654.c:33#1136: goto statement accepted
Newspeak output
---------------
654.c
void main(void) {
  (654.c:26#2)^uint32 goto.lbl1;
  (654.c:26#2)^0- =(uint32) 0;
  (654.c:26#2)^{
    uint32 goto.lbl2;
    (654.c:26#2)^0- =(uint32) 0;
    (654.c:26#2)^do {
      (654.c:26#2)^while (1) {
        (654.c:26#2)^choose {
         -->
          (654.c:26#2)^guard(! (0-_uint32 ==_uint32 0));
         -->
          (654.c:26#2)^guard((0-_uint32 ==_uint32 0));
        }
        (654.c:26#2)^0- =(uint32) 1;
        (654.c:26#2)^choose {
         -->
          (654.c:26#2)^guard(0-_int32);
         -->
          (654.c:26#2)^guard(! 0-_int32);
          (654.c:26#2)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


