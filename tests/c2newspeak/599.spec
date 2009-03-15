Warning: 599.c:32#1106: goto statement accepted
Newspeak output
---------------
599.c
void main(void) {
  (599.c:27#6)^uint32 goto.lbl;
  (599.c:27#6)^0- =(uint32) 0;
  (599.c:27#6)^{
    int32 i;
    (599.c:28#2)^do {
      (599.c:28#2)^while (1) {
        (599.c:28#2)^1- =(uint32) 1-_uint32;
        (599.c:28#9)^choose {
         -->
          (599.c:28#9)^guard(1-_uint32);
          (599.c:28#9)^choose {
           -->
            (599.c:28#9)^guard(! (0-_int32 ==_int32 0));
            (599.c:29#7)^0- =(int32) 1;
           -->
            (599.c:28#9)^guard((0-_int32 ==_int32 0));
            (599.c:32#6)^1- =(uint32) 1;
          }
         -->
          (599.c:28#9)^guard(! 1-_uint32);
          (599.c:32#6)^1- =(uint32) 1;
        }
        (599.c:28#2)^1- =(uint32) 1-_uint32;
        (599.c:28#2)^choose {
         -->
          (599.c:28#2)^guard(1-_uint32);
         -->
          (599.c:28#2)^guard(! 1-_uint32);
          (599.c:28#2)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


