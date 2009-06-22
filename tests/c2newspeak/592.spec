Warning: 592.c:31#1105: goto statement accepted
Newspeak output
---------------
592.c
void main(void) {
  (592.c:27#6)^uint32 goto.lbl;
  (592.c:27#6)^0- =(uint32) 0;
  (592.c:27#6)^{
    int32 i;
    (592.c:28#1)^do {
      (592.c:28#1)^while (1) {
        (592.c:29#2)^0- =(int32) 1;
        (592.c:29#2)^do {
          (592.c:30#2)^do {
            (592.c:29#2)^choose {
             -->
              (592.c:29#2)^guard((0-_int32 ==_int32 1));
              (592.c:30#2)^goto lbl5;
             -->
              (592.c:29#2)^guard(! (0-_int32 ==_int32 1));
              (592.c:29#2)^goto lbl4;
            }
          } with lbl5: {
          }
          (592.c:31#4)^1- =(uint32) 1;
          (592.c:31#4)^choose {
           -->
            (592.c:31#4)^guard(1-_int32);
            (592.c:31#4)^goto lbl4;
           -->
            (592.c:31#4)^guard(! 1-_int32);
          }
        } with lbl4: {
        }
        (592.c:28#1)^choose {
         -->
          (592.c:28#1)^guard(1-_int32);
         -->
          (592.c:28#1)^guard(! 1-_int32);
          (592.c:28#1)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


