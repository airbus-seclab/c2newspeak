Warning: 629.c:29#1068: halting condition should be explicit
Warning: 629.c:41#1184: goto statement accepted
Warning: 629.c:46#1234: init statement expected
Warning: 629.c:48#1260: goto statement accepted
Newspeak output
---------------
629.c
void main(void) {
  (629.c:28#2)^uint32 continue.629.c:38#3.0;
  (629.c:28#2)^0- =(uint32) 0;
  (629.c:28#2)^{
    uint32 goto.lbl1;
    (629.c:28#2)^0- =(uint32) 0;
    (629.c:28#2)^{
      uint32 goto.lbl2;
      (629.c:28#2)^0- =(uint32) 0;
      (629.c:28#2)^while (1) {
        (629.c:28#2)^do {
          (629.c:46#4)^while (1) {
            (629.c:29#4)^0- =(uint32) 0-_uint32;
            (629.c:29#4)^while (1) {
              (629.c:29#4)^choose {
               -->
                (629.c:29#4)^guard(0-_uint32);
               -->
                (629.c:29#4)^guard(! 0-_uint32);
              }
              (629.c:36#6)^0- =(uint32) 0-_uint32;
              (629.c:36#6)^do {
                (629.c:36#6)^while (1) {
                  (629.c:30#6)^0- =(uint32) 0-_uint32;
                  (629.c:30#6)^choose {
                   -->
                    (629.c:30#6)^guard(! 0-_uint32);
                    (629.c:30#6)^1- =(uint32) 1-_uint32;
                   -->
                    (629.c:30#6)^guard(0-_uint32);
                  }
                  (629.c:30#11)^choose {
                   -->
                    (629.c:30#11)^guard(0-_uint32);
                    (629.c:30#11)^do {
                      (629.c:30#11)^choose {
                       -->
                        (629.c:30#11)^guard(1-_uint32);
                        (629.c:30#11)^goto lbl9;
                       -->
                        (629.c:30#11)^guard(! 1-_uint32);
                      }
                    } with lbl9: {
                      (629.c:31#1)^0- =(uint32) 0-_uint32;
                      (629.c:31#1)^choose {
                       -->
                        (629.c:31#1)^guard(! 0-_uint32);
                        (629.c:31#1)^1- =(uint32) 1-_uint32;
                       -->
                        (629.c:31#1)^guard(0-_uint32);
                      }
                      (629.c:31#6)^choose {
                       -->
                        (629.c:31#6)^guard(0-_uint32);
                        (629.c:31#6)^do {
                          (629.c:31#6)^choose {
                           -->
                            (629.c:31#6)^guard(1-_uint32);
                            (629.c:31#6)^goto lbl10;
                           -->
                            (629.c:31#6)^guard(! 1-_uint32);
                          }
                        } with lbl10: {
                          (629.c:31#6)^choose {
                           -->
                            (629.c:31#6)^guard(! 0-_uint32);
                           -->
                            (629.c:31#6)^guard(0-_uint32);
                          }
                        }
                       -->
                        (629.c:31#6)^guard(! 0-_uint32);
                      }
                    }
                   -->
                    (629.c:30#11)^guard(! 0-_uint32);
                  }
                  (629.c:38#3)^2- =(uint32) 1;
                  (629.c:36#6)^do {
                    (629.c:38#3)^goto lbl8;
                    (629.c:41#9)^1- =(uint32) 1-_uint32;
                    (629.c:36#6)^1- =(uint32) 1-_uint32;
                    (629.c:36#6)^choose {
                     -->
                      (629.c:36#6)^guard(1-_uint32);
                     -->
                      (629.c:36#6)^guard(! 1-_uint32);
                      (629.c:36#6)^goto lbl7;
                    }
                  } with lbl8: {
                  }
                }
              } with lbl7: {
              }
              (629.c:29#4)^do {
                (629.c:30#6)^choose {
                 -->
                  (629.c:30#6)^guard(2-_uint32);
                  (629.c:30#6)^2- =(uint32) 0;
                  (629.c:30#6)^goto lbl6;
                 -->
                  (629.c:30#6)^guard(! 2-_uint32);
                }
              } with lbl6: {
              }
            }
            (629.c:46#4)^0- =(uint32) 1;
            (629.c:48#1)^0- =(uint32) 0-_uint32;
            (629.c:46#4)^0- =(uint32) 0-_uint32;
            (629.c:46#4)^choose {
             -->
              (629.c:46#4)^guard(0-_uint32);
             -->
              (629.c:46#4)^guard(! 0-_uint32);
              (629.c:46#4)^goto lbl2;
            }
          }
        } with lbl2: {
        }
      }
    }
  }
}


