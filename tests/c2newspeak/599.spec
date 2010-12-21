Warning: 599.c:32#0: goto statement accepted
Newspeak output
---------------
void main(void) {
  (599.c:27#6)^uint32 goto!lbl;
  (599.c:27#6)^goto!lbl =(uint32) 0;
  (599.c:27#6)^{
    int32 i;
    (599.c:28#2)^do {
      (599.c:28#2)^while (1) {
        (599.c:28#9)^choose {
         -->
          (599.c:28#9)^guard(! (goto!lbl_uint32 ==_uint32 0));
          (599.c:28#9)^guard(! (i_int32 ==_int32 0));
          (599.c:29#7)^i =(int32) 1;
         -->
          (599.c:28#9)^choose {
           -->
            (599.c:28#9)^guard(! (goto!lbl_uint32 ==_uint32 0));
            (599.c:28#9)^guard((i_int32 ==_int32 0));
           -->
            (599.c:28#9)^guard((goto!lbl_uint32 ==_uint32 0));
          }
          (599.c:32#6)^goto!lbl =(uint32) 1;
        }
        (599.c:28#2)^choose {
         -->
          (599.c:28#2)^guard(goto!lbl_int32);
         -->
          (599.c:28#2)^guard(! goto!lbl_int32);
          (599.c:28#2)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


