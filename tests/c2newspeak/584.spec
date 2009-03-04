Warning: 584.c:31#1096: goto statement accepted
Newspeak output
---------------
584.c
void main(void) {
  (584.c:27#6)^uint32 goto.lbl;
  (584.c:27#6)^0- =(uint32) 0;
  (584.c:27#6)^{
    int32 i;
    (584.c:28#2)^1- =(uint32) 1-_uint32;
    (584.c:28#9)^choose {
     -->
      (584.c:28#9)^guard(1-_uint32);
      (584.c:28#9)^choose {
       -->
        (584.c:28#9)^guard(! (0-_int32 ==_int32 0));
        (584.c:29#7)^0- =(int32) 1;
       -->
        (584.c:28#9)^guard((0-_int32 ==_int32 0));
      }
     -->
      (584.c:28#9)^guard(! 1-_uint32);
    }
    (584.c:28#2)^1- =(uint32) 1;
    (584.c:28#2)^do {
      (584.c:28#2)^while (1) {
        (584.c:28#2)^choose {
         -->
          (584.c:28#2)^guard(1-_uint32);
         -->
          (584.c:28#2)^guard(! 1-_uint32);
          (584.c:28#2)^goto lbl1;
        }
        (584.c:28#2)^1- =(uint32) 1-_uint32;
        (584.c:28#9)^choose {
         -->
          (584.c:28#9)^guard(1-_uint32);
          (584.c:28#9)^choose {
           -->
            (584.c:28#9)^guard(! (0-_int32 ==_int32 0));
            (584.c:29#7)^0- =(int32) 1;
           -->
            (584.c:28#9)^guard((0-_int32 ==_int32 0));
          }
         -->
          (584.c:28#9)^guard(! 1-_uint32);
        }
        (584.c:28#2)^1- =(uint32) 1;
      }
    } with lbl1: {
    }
  }
}


