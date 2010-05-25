Warning: 788.c:7#0: goto statement accepted
Newspeak output
---------------
788.c
void main(void) {
  (788.c:2#6)^uint32 goto.lbl;
  (788.c:2#6)^0- =(uint32) 0;
  (788.c:2#6)^{
    int32[10] t;
    (788.c:6#10)^int32 q;
    (788.c:6#10)^0- =(int32) 1- + 160_int32;
    (788.c:5#4)^do {
      (788.c:5#4)^while (1) {
        (788.c:3#9)^choose {
         -->
          (788.c:3#9)^guard(! (2-_uint32 ==_uint32 0));
         -->
          (788.c:3#9)^guard((2-_uint32 ==_uint32 0));
        }
        (788.c:5#4)^2- =(uint32) 1;
        (788.c:5#4)^choose {
         -->
          (788.c:5#4)^guard(2-_int32);
         -->
          (788.c:5#4)^guard(! 2-_int32);
          (788.c:5#4)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


