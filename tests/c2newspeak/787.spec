Warning: 787.c:7#0: goto statement accepted
Newspeak output
---------------
void (787.c:1#5)^main(void) {
  (787.c:2#2)^uint32 goto!lbl;
  (787.c:2#2)^goto!lbl =(uint32) 0;
  (787.c:6#8)^{
    int32 q;
    (787.c:5#2)^do {
      (787.c:5#2)^while (1) {
        (787.c:2#9)^choose {
         -->
          (787.c:2#9)^guard(! (goto!lbl_uint32 ==_uint32 0));
         -->
          (787.c:2#9)^guard((goto!lbl_uint32 ==_uint32 0));
        }
        (787.c:5#2)^goto!lbl =(uint32) 1;
        (787.c:5#2)^choose {
         -->
          (787.c:5#2)^guard(goto!lbl_int32);
         -->
          (787.c:5#2)^guard(! goto!lbl_int32);
          (787.c:5#2)^goto lbl1;
        }
      }
    } with lbl1:
    (787.c:8#6)^q =(int32) 0;
  }
}


