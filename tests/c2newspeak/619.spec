Warning: 619.c:29#1080: goto statement accepted
Warning: 619.c:30#1092: goto statement accepted
Newspeak output
---------------
619.c
void main(void) {
  (619.c:27#6)^uint32 goto.lbl;
  (619.c:27#6)^0- =(uint32) 0;
  (619.c:27#6)^{
    int32 i;
    (619.c:28#1)^while (1) {
      (619.c:29#2)^while (1) {
        (619.c:29#2)^0- =(int32) 0;
      }
    }
  }
}

