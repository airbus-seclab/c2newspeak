Warning: 623.c:34#1133: goto statement accepted
Newspeak output
---------------
623.c
void main(void) {
  (623.c:27#2)^uint32 goto.fail;
  (623.c:27#2)^0- =(uint32) 0;
  (623.c:28#4)^0- =(uint32) 0-_uint32;
  (623.c:28#11)^do {
    (623.c:28#11)^choose {
     -->
      (623.c:28#11)^guard(0-_uint32);
      (623.c:28#11)^goto lbl4;
     -->
      (623.c:28#11)^guard(! 0-_uint32);
    }
  } with lbl4: {
    (623.c:29#6)^0- =(uint32) 0-_uint32;
    (623.c:29#12)^choose {
     -->
      (623.c:29#12)^guard(0-_uint32);
     -->
      (623.c:29#12)^guard(! 0-_uint32);
    }
  }
  (623.c:34#6)^0- =(uint32) 1;
  (623.c:33#4)^0- =(uint32) 0-_uint32;
  (623.c:27#2)^do {
    (623.c:33#4)^while (1) {
      (623.c:33#4)^choose {
       -->
        (623.c:33#4)^guard(0-_uint32);
       -->
        (623.c:33#4)^guard(! 0-_uint32);
        (623.c:33#4)^goto lbl2;
      }
      (623.c:28#4)^0- =(uint32) 0-_uint32;
      (623.c:28#11)^do {
        (623.c:28#11)^choose {
         -->
          (623.c:28#11)^guard(0-_uint32);
          (623.c:28#11)^goto lbl7;
         -->
          (623.c:28#11)^guard(! 0-_uint32);
        }
      } with lbl7: {
        (623.c:29#6)^0- =(uint32) 0-_uint32;
        (623.c:29#12)^choose {
         -->
          (623.c:29#12)^guard(0-_uint32);
         -->
          (623.c:29#12)^guard(! 0-_uint32);
        }
      }
      (623.c:34#6)^0- =(uint32) 1;
      (623.c:33#4)^0- =(uint32) 0-_uint32;
    }
  } with lbl2: {
  }
  (623.c:27#2)^while (1) {
    (623.c:28#4)^0- =(uint32) 0-_uint32;
    (623.c:28#11)^do {
      (623.c:28#11)^choose {
       -->
        (623.c:28#11)^guard(0-_uint32);
        (623.c:28#11)^goto lbl11;
       -->
        (623.c:28#11)^guard(! 0-_uint32);
      }
    } with lbl11: {
      (623.c:29#6)^0- =(uint32) 0-_uint32;
      (623.c:29#12)^choose {
       -->
        (623.c:29#12)^guard(0-_uint32);
       -->
        (623.c:29#12)^guard(! 0-_uint32);
      }
    }
    (623.c:34#6)^0- =(uint32) 1;
    (623.c:33#4)^0- =(uint32) 0-_uint32;
    (623.c:27#2)^do {
      (623.c:33#4)^while (1) {
        (623.c:33#4)^choose {
         -->
          (623.c:33#4)^guard(0-_uint32);
         -->
          (623.c:33#4)^guard(! 0-_uint32);
          (623.c:33#4)^goto lbl9;
        }
        (623.c:28#4)^0- =(uint32) 0-_uint32;
        (623.c:28#11)^do {
          (623.c:28#11)^choose {
           -->
            (623.c:28#11)^guard(0-_uint32);
            (623.c:28#11)^goto lbl14;
           -->
            (623.c:28#11)^guard(! 0-_uint32);
          }
        } with lbl14: {
          (623.c:29#6)^0- =(uint32) 0-_uint32;
          (623.c:29#12)^choose {
           -->
            (623.c:29#12)^guard(0-_uint32);
           -->
            (623.c:29#12)^guard(! 0-_uint32);
          }
        }
        (623.c:34#6)^0- =(uint32) 1;
        (623.c:33#4)^0- =(uint32) 0-_uint32;
      }
    } with lbl9: {
    }
  }
}


