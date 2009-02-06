Warning: 567.c:29#1080: goto statement accepted
Newspeak output
---------------
567.c
void main(void) {
  (567.c:27#6)^uint32 goto.lbl;
  (567.c:27#6)^0- =(uint32) 0;
  (567.c:27#6)^{
    int32 i;
    (567.c:28#6)^1- =(uint32) 0;
    (567.c:28#6)^0- =(int32) 0;
    (567.c:28#1)^do {
      (567.c:28#1)^while (1) {
        (567.c:28#1)^choose {
          | (0 ==_uint32 0) -->
          | ! (0 ==_uint32 0) -->
            (567.c:28#1)^goto lbl1;
        }
        (567.c:28#6)^1- =(uint32) 0;
        (567.c:28#6)^0- =(int32) 0;
      }
    } with lbl1: {
    }
  }
}


