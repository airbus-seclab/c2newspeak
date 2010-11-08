Warning: 585.c:34#0: goto statement accepted
Newspeak output
---------------
585.c
void main(void) {
  (585.c:27#6)^uint32 goto!lbl;
  (585.c:27#6)^goto!lbl =(uint32) 0;
  (585.c:27#6)^{
    int32 i;
    (585.c:28#2)^do {
      (585.c:28#2)^while (1) {
        (585.c:31#6)^choose {
         -->
          (585.c:31#6)^guard((goto!lbl_uint32 ==_uint32 0));
          (585.c:31#6)^guard((i_int32 ==_int32 0));
          (585.c:29#4)^i =(int32) coerce[-2147483648,2147483647] (i_int32 + 1);
         -->
          (585.c:31#6)^choose {
           -->
            (585.c:31#6)^guard((goto!lbl_uint32 ==_uint32 0));
            (585.c:31#6)^guard(! (i_int32 ==_int32 0));
           -->
            (585.c:31#6)^guard(! (goto!lbl_uint32 ==_uint32 0));
          }
          (585.c:32#7)^i =(int32) 1;
        }
        (585.c:28#2)^goto!lbl =(uint32) 1;
        (585.c:28#2)^choose {
         -->
          (585.c:28#2)^guard(goto!lbl_int32);
         -->
          (585.c:28#2)^guard(! goto!lbl_int32);
          (585.c:28#2)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


