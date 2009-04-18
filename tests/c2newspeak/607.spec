Warning: 607.c:35#1144: goto statement accepted
Newspeak output
---------------
607.c
void main(void) {
  (607.c:27#6)^uint32 goto.lbl;
  (607.c:27#6)^0- =(uint32) 0;
  (607.c:27#6)^{
    int32 i;
    (607.c:28#2)^do {
      (607.c:28#2)^while (1) {
        (607.c:28#2)^while (1) {
          (607.c:28#2)^choose {
           -->
            (607.c:28#2)^guard(1-_uint32);
           -->
            (607.c:28#2)^guard(! 1-_uint32);
          }
          (607.c:29#4)^choose {
           -->
            (607.c:29#4)^guard(! 1-_uint32);
            (607.c:29#4)^0- =(int32) 1;
           -->
            (607.c:29#4)^guard(1-_uint32);
          }
          (607.c:30#11)^choose {
           -->
            (607.c:30#11)^guard(1-_uint32);
            (607.c:30#11)^guard(! (0-_int32 ==_int32 0));
            (607.c:30#11)^choose {
             -->
              (607.c:30#11)^guard(! 1-_uint32);
              (607.c:31#6)^0- =(int32) 2;
             -->
              (607.c:30#11)^guard(1-_uint32);
            }
            (607.c:32#9)^0- =(int32) 0;
           -->
            (607.c:30#11)^choose {
             -->
              (607.c:30#11)^guard(1-_uint32);
              (607.c:30#11)^guard((0-_int32 ==_int32 0));
             -->
              (607.c:30#11)^guard(! 1-_uint32);
            }
          }
        }
        (607.c:28#2)^1- =(uint32) 1;
        (607.c:28#2)^choose {
         -->
          (607.c:28#2)^guard(1-_uint32);
         -->
          (607.c:28#2)^guard(! 1-_uint32);
          (607.c:28#2)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


