Warning: 590.c:32#1104: goto statement accepted
Newspeak output
---------------
590.c
void main(void) {
  (590.c:27#6)^uint32 goto.lbl;
  (590.c:27#6)^0- =(uint32) 0;
  (590.c:27#6)^{
    int32 i;
    (590.c:29#2)^0- =(int32) 1;
    (590.c:29#2)^choose {
     -->
      (590.c:29#2)^guard(! (0-_int32 ==_int32 0));
     -->
      (590.c:29#2)^guard((0-_int32 ==_int32 0));
      (590.c:32#4)^1- =(uint32) 1;
    }
    (590.c:28#1)^do {
      (590.c:28#1)^while (1) {
        (590.c:28#1)^choose {
         -->
          (590.c:28#1)^guard(1-_uint32);
         -->
          (590.c:28#1)^guard(! 1-_uint32);
          (590.c:28#1)^goto lbl1;
        }
        (590.c:29#2)^0- =(int32) 1;
        (590.c:29#2)^choose {
         -->
          (590.c:29#2)^guard(! (0-_int32 ==_int32 0));
         -->
          (590.c:29#2)^guard((0-_int32 ==_int32 0));
          (590.c:32#4)^1- =(uint32) 1;
        }
      }
    } with lbl1: {
    }
  }
}

