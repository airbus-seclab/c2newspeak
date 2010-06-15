Warning: 712.c:35#0: goto statement accepted
Newspeak output
---------------
712.c
void main(void) {
  (712.c:32#2)^uint32 goto!lbl;
  (712.c:32#2)^goto!lbl =(uint32) 0;
  (712.c:32#2)^do {
    (712.c:32#2)^while (1) {
      (712.c:32#9)^choose {
       -->
        (712.c:32#9)^guard(! (goto!lbl_uint32 ==_uint32 0));
       -->
        (712.c:32#9)^guard((goto!lbl_uint32 ==_uint32 0));
      }
      (712.c:32#2)^goto!lbl =(uint32) 1;
      (712.c:32#2)^choose {
       -->
        (712.c:32#2)^guard(goto!lbl_int32);
       -->
        (712.c:32#2)^guard(! goto!lbl_int32);
        (712.c:32#2)^goto lbl1;
      }
    }
  } with lbl1: {
  }
}


