Warning: 618.c:32#1134: goto statement accepted
Newspeak output
---------------
618.c
void main(void) {
  (618.c:27#4)^uint32 goto.lbl;
  (618.c:27#4)^0- =(uint32) 0;
  (618.c:27#4)^while (1) {
    (618.c:28#6)^uint32 continue.618.c:31#13.0;
    (618.c:28#6)^0- =(uint32) 0;
    (618.c:28#6)^1- =(uint32) 1-_uint32;
    (618.c:28#6)^while (1) {
      (618.c:28#6)^choose {
       -->
        (618.c:28#6)^guard(1-_uint32);
       -->
        (618.c:28#6)^guard(! 1-_uint32);
      }
      (618.c:29#6)^1- =(uint32) 0;
    }
    (618.c:31#13)^0- =(uint32) 1;
    (618.c:31#6)^do {
      (618.c:31#13)^goto lbl5;
      (618.c:31#6)^1- =(uint32) 1;
    } with lbl5: {
    }
    (618.c:31#6)^do {
      (618.c:31#6)^while (1) {
        (618.c:31#6)^choose {
         -->
          (618.c:31#6)^guard(1-_uint32);
         -->
          (618.c:31#6)^guard(! 1-_uint32);
          (618.c:31#6)^goto lbl4;
        }
        (618.c:28#6)^1- =(uint32) 1-_uint32;
        (618.c:28#6)^while (1) {
          (618.c:28#6)^choose {
           -->
            (618.c:28#6)^guard(1-_uint32);
           -->
            (618.c:28#6)^guard(! 1-_uint32);
          }
          (618.c:29#6)^1- =(uint32) 0;
        }
        (618.c:31#13)^0- =(uint32) 1;
        (618.c:31#6)^do {
          (618.c:31#13)^goto lbl10;
          (618.c:31#6)^1- =(uint32) 1;
        } with lbl10: {
        }
      }
    } with lbl4: {
    }
    (618.c:27#4)^do {
      (618.c:28#6)^choose {
       -->
        (618.c:28#6)^guard(0-_uint32);
        (618.c:28#6)^0- =(uint32) 0;
        (618.c:28#6)^goto lbl3;
       -->
        (618.c:28#6)^guard(! 0-_uint32);
      }
    } with lbl3: {
    }
  }
}


