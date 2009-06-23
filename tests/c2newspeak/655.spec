Warning: 655.c:30#1102: goto statement accepted
Warning: 655.c:33#1136: goto statement accepted
Warning: 655.c:35#1155: goto statement accepted
Warning: 655.c:36#1168: goto statement accepted
Newspeak output
---------------
655.c
void main(void) {
  (655.c:26#6)^uint32 goto.lbl1;
  (655.c:26#6)^0- =(uint32) 0;
  (655.c:26#6)^{
    uint32 goto.lbl2;
    (655.c:26#6)^0- =(uint32) 0;
    (655.c:26#6)^{
      int32 i;
      (655.c:27#2)^do {
        (655.c:28#2)^do {
          (655.c:27#2)^choose {
           -->
            (655.c:27#2)^guard((0-_int32 ==_int32 2));
            (655.c:28#2)^goto lbl4;
           -->
            (655.c:27#2)^guard(! (0-_int32 ==_int32 2));
            (655.c:27#2)^goto lbl3;
          }
        } with lbl4: {
        }
        (655.c:30#6)^2- =(uint32) 1;
      } with lbl3: {
      }
      (655.c:33#4)^2- =(uint32) 1;
      (655.c:27#2)^do {
        (655.c:33#4)^choose {
         -->
          (655.c:33#4)^guard(2-_int32);
          (655.c:33#4)^goto lbl2;
         -->
          (655.c:33#4)^guard(! 2-_int32);
        }
      } with lbl2: {
      }
      (655.c:33#4)^choose {
       -->
        (655.c:33#4)^guard(! 2-_int32);
        (655.c:36#2)^1- =(uint32) 1;
        (655.c:37#1)^choose {
         -->
          (655.c:37#1)^guard(! (1-_uint32 ==_uint32 0));
         -->
          (655.c:37#1)^guard((1-_uint32 ==_uint32 0));
        }
       -->
        (655.c:33#4)^guard(2-_int32);
      }
    }
  }
}


