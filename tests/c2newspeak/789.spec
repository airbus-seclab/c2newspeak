Warning: 789.c:7#0: goto statement accepted
Warning: 789.c:9#0: goto statement accepted
Newspeak output
---------------
789.c
void main(void) {
  (789.c:2#2)^uint32 goto.lbl1;
  (789.c:2#2)^0- =(uint32) 0;
  (789.c:2#2)^{
    uint32 goto.lbl2;
    (789.c:2#2)^0- =(uint32) 0;
    (789.c:12#2)^do {
      (789.c:5#2)^while (1) {
        (789.c:2#9)^choose {
         -->
          (789.c:2#9)^guard(! (1-_uint32 ==_uint32 0));
         -->
          (789.c:2#9)^guard((1-_uint32 ==_uint32 0));
        }
        (789.c:5#2)^1- =(uint32) 1;
        (789.c:5#2)^choose {
         -->
          (789.c:5#2)^guard(1-_int32);
         -->
          (789.c:5#2)^guard(! 1-_int32);
          (789.c:5#2)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


