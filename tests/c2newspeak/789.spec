Warning: 789.c:7#0: goto statement accepted
Warning: 789.c:9#0: goto statement accepted
Newspeak output
---------------
void (789.c:1#5)^main(void) {
  (789.c:2#2)^uint32 goto!lbl1;
  (789.c:2#2)^goto!lbl1 =(uint32) 0;
  (789.c:2#2)^{
    uint32 goto!lbl2;
    (789.c:2#2)^goto!lbl2 =(uint32) 0;
    (789.c:12#2)^do {
      (789.c:5#2)^while (1) {
        (789.c:2#9)^choose {
         -->
          (789.c:2#9)^guard(! (goto!lbl1_uint32 ==_uint32 0));
         -->
          (789.c:2#9)^guard((goto!lbl1_uint32 ==_uint32 0));
        }
        (789.c:5#2)^goto!lbl1 =(uint32) 1;
        (789.c:5#2)^choose {
         -->
          (789.c:5#2)^guard(goto!lbl1_int32);
         -->
          (789.c:5#2)^guard(! goto!lbl1_int32);
          (789.c:5#2)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


