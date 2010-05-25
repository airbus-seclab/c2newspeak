Warning: 787.c:7#0: goto statement accepted
Newspeak output
---------------
787.c
void main(void) {
  (787.c:6#8)^int32 q;
  (787.c:2#2)^uint32 goto.lbl;
  (787.c:2#2)^0- =(uint32) 0;
  (787.c:2#2)^do {
    (787.c:2#2)^while (1) {
      (787.c:2#9)^choose {
       -->
        (787.c:2#9)^guard(! (0-_uint32 ==_uint32 0));
       -->
        (787.c:2#9)^guard((0-_uint32 ==_uint32 0));
      }
      (787.c:2#2)^0- =(uint32) 1;
      (787.c:2#2)^choose {
       -->
        (787.c:2#2)^guard(0-_int32);
       -->
        (787.c:2#2)^guard(! 0-_int32);
        (787.c:2#2)^goto lbl1;
      }
    }
  } with lbl1: {
  }
  (787.c:8#6)^1- =(int32) 0;
}


