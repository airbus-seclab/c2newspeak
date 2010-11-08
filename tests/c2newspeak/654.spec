Warning: 654.c:27#0: goto statement accepted
Warning: 654.c:33#0: goto statement accepted
Newspeak output
---------------
654.c
void main(void) {
  (654.c:26#2)^uint32 goto!lbl1;
  (654.c:26#2)^goto!lbl1 =(uint32) 0;
  (654.c:26#2)^{
    uint32 goto!lbl2;
    (654.c:26#2)^goto!lbl2 =(uint32) 0;
    (654.c:26#2)^do {
      (654.c:26#2)^while (1) {
        (654.c:26#2)^choose {
         -->
          (654.c:26#2)^guard(! (goto!lbl2_uint32 ==_uint32 0));
         -->
          (654.c:26#2)^guard((goto!lbl2_uint32 ==_uint32 0));
        }
        (654.c:26#2)^goto!lbl2 =(uint32) 1;
        (654.c:26#2)^choose {
         -->
          (654.c:26#2)^guard(goto!lbl2_int32);
         -->
          (654.c:26#2)^guard(! goto!lbl2_int32);
          (654.c:26#2)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


