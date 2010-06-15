Warning: 637.c:26#0: goto statement accepted
Newspeak output
---------------
637.c
void main(void) {
  (637.c:26#4)^uint32 goto!lbl;
  (637.c:26#4)^goto!lbl =(uint32) 0;
  (637.c:28#10)^{
    int32 j;
    (637.c:27#4)^goto!lbl =(uint32) 1;
    (637.c:27#4)^while (1) {
      (637.c:27#4)^choose {
       -->
        (637.c:27#4)^guard(! (goto!lbl_uint32 ==_uint32 0));
       -->
        (637.c:27#4)^guard((goto!lbl_uint32 ==_uint32 0));
      }
      (637.c:29#6)^while (1) {
        (637.c:29#8)^choose {
         -->
          (637.c:29#8)^guard(! goto!lbl_int32);
          (637.c:30#1)^j =(int32) 0;
         -->
          (637.c:29#8)^guard(goto!lbl_int32);
        }
        (637.c:31#6)^goto!lbl =(uint32) 0;
      }
    }
  }
}


