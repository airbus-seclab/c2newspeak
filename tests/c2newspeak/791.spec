Warning: 791.c:7#2: goto statement accepted
Newspeak output
---------------
void (791.c:1#5)^main(void) {
  (791.c:2#2)^uint32 goto!lbl;
  (791.c:2#2)^goto!lbl =(uint32) 0;
  (791.c:2#2)^do {
    (791.c:2#2)^while (1) {
      (791.c:3#11)^choose {
       -->
        (791.c:3#11)^guard(! (goto!lbl_uint32 ==_uint32 0));
       -->
        (791.c:3#11)^guard((goto!lbl_uint32 ==_uint32 0));
      }
      (791.c:2#2)^goto!lbl =(uint32) 1;
      (791.c:2#2)^choose {
       -->
        (791.c:2#2)^guard(goto!lbl_int32);
       -->
        (791.c:2#2)^guard(! goto!lbl_int32);
        (791.c:2#2)^goto lbl1;
      }
    }
  } with lbl1:
}


