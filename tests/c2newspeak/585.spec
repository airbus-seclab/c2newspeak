Warning: 585.c:34#0: goto statement accepted
Newspeak output
---------------
585.c
void main(void) {
  (585.c:27#6)^uint32 goto.lbl;
  (585.c:27#6)^0- =(uint32) 0;
  (585.c:27#6)^{
    int32 i;
    (585.c:28#2)^do {
      (585.c:28#2)^while (1) {
        (585.c:31#6)^choose {
         -->
          (585.c:31#6)^guard((1-_uint32 ==_uint32 0));
          (585.c:31#6)^guard((0-_int32 ==_int32 0));
          (585.c:29#4)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
         -->
          (585.c:31#6)^choose {
           -->
            (585.c:31#6)^guard((1-_uint32 ==_uint32 0));
            (585.c:31#6)^guard(! (0-_int32 ==_int32 0));
           -->
            (585.c:31#6)^guard(! (1-_uint32 ==_uint32 0));
          }
          (585.c:32#7)^0- =(int32) 1;
        }
        (585.c:28#2)^1- =(uint32) 1;
        (585.c:28#2)^choose {
         -->
          (585.c:28#2)^guard(1-_int32);
         -->
          (585.c:28#2)^guard(! 1-_int32);
          (585.c:28#2)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


