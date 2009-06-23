Warning: 588.c:30#1091: goto statement accepted
Newspeak output
---------------
588.c
void main(void) {
  (588.c:27#6)^uint32 goto.lbl;
  (588.c:27#6)^0- =(uint32) 0;
  (588.c:27#6)^{
    int32 i;
    (588.c:28#1)^do {
      (588.c:28#1)^while (1) {
        (588.c:29#2)^0- =(int32) 1;
        (588.c:29#2)^choose {
         -->
          (588.c:29#2)^guard(! (0-_int32 ==_int32 0));
          (588.c:29#2)^1- =(uint32) 1;
         -->
          (588.c:29#2)^guard((0-_int32 ==_int32 0));
        }
        (588.c:28#1)^choose {
         -->
          (588.c:28#1)^guard(1-_int32);
         -->
          (588.c:28#1)^guard(! 1-_int32);
          (588.c:28#1)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


