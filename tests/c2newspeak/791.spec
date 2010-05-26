Warning: 791.c:7#0: goto statement accepted
Newspeak output
---------------
791.c
void main(void) {
  (791.c:2#2)^uint32 goto.lbl;
  (791.c:2#2)^0- =(uint32) 0;
  (791.c:2#2)^do {
    (791.c:2#2)^while (1) {
      (791.c:3#11)^choose {
       -->
        (791.c:3#11)^guard(! (0-_uint32 ==_uint32 0));
       -->
        (791.c:3#11)^guard((0-_uint32 ==_uint32 0));
      }
      (791.c:2#2)^0- =(uint32) 1;
      (791.c:2#2)^choose {
       -->
        (791.c:2#2)^guard(0-_int32);
       -->
        (791.c:2#2)^guard(! 0-_int32);
        (791.c:2#2)^goto lbl1;
      }
    }
  } with lbl1: {
  }
}


