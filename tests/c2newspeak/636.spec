Warning: 636.c:27#0: goto statement accepted
Newspeak output
---------------
636.c
void main(void) {
  (636.c:26#2)^uint32 goto.lbl;
  (636.c:26#2)^goto.lbl =(uint32) 0;
  (636.c:28#4)^goto.lbl =(uint32) 1;
  (636.c:28#9)^choose {
   -->
    (636.c:28#9)^guard(! (goto.lbl_uint32 ==_uint32 0));
   -->
    (636.c:28#9)^guard((goto.lbl_uint32 ==_uint32 0));
  }
}


