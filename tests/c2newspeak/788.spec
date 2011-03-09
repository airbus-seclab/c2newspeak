Warning: 788.c:7#13: goto statement accepted
Newspeak output
---------------
void (788.c:1#5)^main(void) {
  (788.c:2#6)^uint32 goto!lbl;
  (788.c:2#6)^goto!lbl =(uint32) 0;
  (788.c:2#6)^{
    int32[10] t;
    (788.c:6#10)^int32 q;
    (788.c:6#10)^q =(int32) t + 160_int32;
    (788.c:4#2)^do {
      (788.c:4#2)^while (1) {
        (788.c:3#9)^choose {
         -->
          (788.c:3#9)^guard(! (goto!lbl_uint32 ==_uint32 0));
         -->
          (788.c:3#9)^guard((goto!lbl_uint32 ==_uint32 0));
        }
        (788.c:4#2)^goto!lbl =(uint32) 1;
        (788.c:4#2)^choose {
         -->
          (788.c:4#2)^guard(goto!lbl_int32);
         -->
          (788.c:4#2)^guard(! goto!lbl_int32);
          (788.c:4#2)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


