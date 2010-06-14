Warning: 652.c:29#0: goto statement accepted
Newspeak output
---------------
652.c
void main(void) {
  (652.c:26#2)^uint32 goto.lbl;
  (652.c:26#2)^goto.lbl =(uint32) 0;
  (652.c:27#8)^{
    int32 i;
    (652.c:29#6)^choose {
     -->
      (652.c:29#6)^guard(! goto.lbl_int32);
      (652.c:29#6)^goto.lbl =(uint32) 1;
     -->
      (652.c:29#6)^guard(goto.lbl_int32);
    }
    (652.c:32#4)^i =(int32) coerce[-2147483648,2147483647] (i_int32 + 1);
  }
}


