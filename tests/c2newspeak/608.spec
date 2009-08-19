Warning: 608.c:28#0: goto statement accepted
Newspeak output
---------------
608.c
void main(void) {
  (608.c:27#6)^uint32 goto.lbl;
  (608.c:27#6)^0- =(uint32) 0;
  (608.c:27#6)^{
    int32 i;
    (608.c:29#2)^1- =(uint32) 1;
    (608.c:29#2)^while (1) {
      (608.c:29#2)^choose {
       -->
        (608.c:29#2)^guard(! (1-_uint32 ==_uint32 0));
       -->
        (608.c:29#2)^guard((1-_uint32 ==_uint32 0));
      }
      (608.c:30#2)^1- =(uint32) 0;
    }
  }
}


