Warning: 609.c:30#0: goto statement accepted
Newspeak output
---------------
609.c
void main(void) {
  (609.c:27#6)^uint32 goto!lbl;
  (609.c:27#6)^goto!lbl =(uint32) 0;
  (609.c:27#6)^{
    int32 i;
    (609.c:30#6)^goto!lbl =(uint32) 1;
    (609.c:30#6)^choose {
     -->
      (609.c:30#6)^guard(! goto!lbl_int32);
      (609.c:31#6)^i =(int32) 2;
     -->
      (609.c:30#6)^guard(goto!lbl_int32);
    }
    (609.c:30#6)^choose {
     -->
      (609.c:30#6)^guard(! goto!lbl_int32);
      (609.c:33#4)^i =(int32) 1;
     -->
      (609.c:30#6)^guard(goto!lbl_int32);
    }
    (609.c:35#2)^while (1) {
      (609.c:35#2)^choose {
       -->
        (609.c:35#2)^guard(! (goto!lbl_uint32 ==_uint32 0));
       -->
        (609.c:35#2)^guard((goto!lbl_uint32 ==_uint32 0));
      }
      (609.c:36#2)^goto!lbl =(uint32) 0;
    }
  }
}


