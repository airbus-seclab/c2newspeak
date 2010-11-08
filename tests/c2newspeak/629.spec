Warning: 629.c:29#0: halting condition should be explicit
Warning: 629.c:41#0: goto statement accepted
Warning: 629.c:45#0: init statement expected
Warning: 629.c:48#0: goto statement accepted
Newspeak output
---------------
629.c
void main(void) {
  (629.c:28#2)^uint32 continue.629.c:38#3.0;
  (629.c:28#2)^continue.629.c:38#3.0 =(uint32) 0;
  (629.c:28#2)^{
    uint32 goto!lbl1;
    (629.c:28#2)^goto!lbl1 =(uint32) 0;
    (629.c:28#2)^{
      uint32 goto!lbl2;
      (629.c:28#2)^goto!lbl2 =(uint32) 0;
      (629.c:28#2)^while (1) {
        (629.c:28#2)^do {
          (629.c:46#4)^while (1) {
            (629.c:29#4)^while (1) {
              (629.c:29#4)^choose {
               -->
                (629.c:29#4)^guard(! (goto!lbl2_uint32 ==_uint32 0));
               -->
                (629.c:29#4)^guard((goto!lbl2_uint32 ==_uint32 0));
              }
              (629.c:36#6)^do {
                (629.c:36#6)^while (1) {
                  (629.c:30#11)^choose {
                   -->
                    (629.c:30#11)^guard(! (goto!lbl2_uint32 ==_uint32 0));
                    (629.c:30#11)^guard(! (goto!lbl1_uint32 ==_uint32 0));
                    (629.c:31#6)^choose {
                     -->
                      (629.c:31#6)^guard(! (goto!lbl2_uint32 ==_uint32 0));
                      (629.c:31#6)^guard(! (goto!lbl1_uint32 ==_uint32 0));
                      (629.c:31#6)^choose {
                       -->
                        (629.c:31#6)^guard(! goto!lbl2_int32);
                       -->
                        (629.c:31#6)^guard(goto!lbl2_int32);
                      }
                     -->
                      (629.c:31#6)^choose {
                       -->
                        (629.c:31#6)^guard(! (goto!lbl2_uint32 ==_uint32 0));
                        (629.c:31#6)^guard((goto!lbl1_uint32 ==_uint32 0));
                       -->
                        (629.c:31#6)^guard((goto!lbl2_uint32 ==_uint32 0));
                      }
                    }
                   -->
                    (629.c:30#11)^choose {
                     -->
                      (629.c:30#11)^guard(! (goto!lbl2_uint32 ==_uint32 0));
                      (629.c:30#11)^guard((goto!lbl1_uint32 ==_uint32 0));
                     -->
                      (629.c:30#11)^guard((goto!lbl2_uint32 ==_uint32 0));
                    }
                  }
                  (629.c:38#3)^continue.629.c:38#3.0 =(uint32) 1;
                  (629.c:36#6)^do {
                    (629.c:38#3)^goto lbl8;
                    (629.c:36#6)^choose {
                     -->
                      (629.c:36#6)^guard(goto!lbl1_int32);
                     -->
                      (629.c:36#6)^guard(! goto!lbl1_int32);
                      (629.c:36#6)^goto lbl7;
                    }
                  } with lbl8:
                }
              } with lbl7:
              (629.c:29#4)^do {
                (629.c:30#6)^choose {
                 -->
                  (629.c:30#6)^guard(continue.629.c:38#3.0_int32);
                  (629.c:30#6)^continue.629.c:38#3.0 =(uint32) 0;
                  (629.c:30#6)^goto lbl6;
                 -->
                  (629.c:30#6)^guard(! continue.629.c:38#3.0_int32);
                }
              } with lbl6:
            }
            (629.c:46#4)^goto!lbl2 =(uint32) 1;
            (629.c:46#4)^choose {
             -->
              (629.c:46#4)^guard(goto!lbl2_int32);
             -->
              (629.c:46#4)^guard(! goto!lbl2_int32);
              (629.c:46#4)^goto lbl2;
            }
          }
        } with lbl2:
      }
    }
  }
}


