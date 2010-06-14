Warning: 619.c:29#0: goto statement accepted
Warning: 619.c:30#0: goto statement accepted
Newspeak output
---------------
619.c
void main(void) {
  (619.c:27#6)^uint32 goto.lbl;
  (619.c:27#6)^goto.lbl =(uint32) 0;
  (619.c:27#6)^{
    int32 i;
    (619.c:28#1)^while (1) {
      (619.c:29#2)^while (1) {
        (619.c:29#2)^i =(int32) 0;
      }
    }
  }
}


