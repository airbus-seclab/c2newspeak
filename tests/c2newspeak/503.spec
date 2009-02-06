Newspeak output
---------------
503.c
void main(void) {
  (503.c:29#2)^do {
    (503.c:29#2)^do {
      (503.c:29#2)^choose {
       -->
        (503.c:29#2)^guard(! (a_int32 ==_int32 0));
        (503.c:29#2)^choose {
         -->
          (503.c:29#2)^guard(! (b_int32 ==_int32 0));
          (503.c:29#2)^choose {
           -->
            (503.c:29#2)^guard(! (d_int32 ==_int32 0));
            (503.c:29#2)^goto lbl2;
           -->
            (503.c:29#2)^guard((d_int32 ==_int32 0));
            (503.c:29#2)^goto lbl1;
          }
         -->
          (503.c:29#2)^guard((b_int32 ==_int32 0));
          (503.c:29#2)^choose {
           -->
            (503.c:29#2)^guard(! (e_int32 ==_int32 0));
            (503.c:29#2)^goto lbl2;
           -->
            (503.c:29#2)^guard((e_int32 ==_int32 0));
            (503.c:29#2)^goto lbl1;
          }
        }
       -->
        (503.c:29#2)^guard((a_int32 ==_int32 0));
        (503.c:29#2)^choose {
         -->
          (503.c:29#2)^guard(! (c_int32 ==_int32 0));
          (503.c:29#2)^choose {
           -->
            (503.c:29#2)^guard(! (d_int32 ==_int32 0));
            (503.c:29#2)^goto lbl2;
           -->
            (503.c:29#2)^guard((d_int32 ==_int32 0));
            (503.c:29#2)^goto lbl1;
          }
         -->
          (503.c:29#2)^guard((c_int32 ==_int32 0));
          (503.c:29#2)^choose {
           -->
            (503.c:29#2)^guard(! (e_int32 ==_int32 0));
            (503.c:29#2)^goto lbl2;
           -->
            (503.c:29#2)^guard((e_int32 ==_int32 0));
            (503.c:29#2)^goto lbl1;
          }
        }
      }
    } with lbl2: {
      (503.c:30#4)^x =(int32) 0;
      (503.c:31#4)^x =(int32) 1;
      (503.c:32#4)^x =(int32) 2;
      (503.c:33#4)^x =(int32) 3;
      (503.c:34#4)^x =(int32) 4;
    }
  } with lbl1: {
    (503.c:36#4)^x =(int32) 0;
    (503.c:37#4)^x =(int32) 1;
    (503.c:38#4)^x =(int32) 2;
    (503.c:39#4)^x =(int32) 3;
    (503.c:40#4)^x =(int32) 4;
    (503.c:41#4)^x =(int32) 5;
  }
}

int32 a = 0;
int32 b = 0;
int32 c = 0;
int32 d = 0;
int32 e = 0;
int32 x = 0;

