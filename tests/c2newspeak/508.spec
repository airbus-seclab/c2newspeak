Newspeak output
---------------
508.c
void main(void) {
  (508.c:29#2)^do {
    (508.c:29#2)^do {
      (508.c:29#2)^choose {
       -->
        (508.c:29#2)^guard(! (a_int32 ==_int32 0));
        (508.c:29#2)^choose {
         -->
          (508.c:29#2)^guard(! (b_int32 ==_int32 0));
          (508.c:29#2)^goto lbl1;
         -->
          (508.c:29#2)^guard((b_int32 ==_int32 0));
          (508.c:29#2)^goto lbl2;
        }
       -->
        (508.c:29#2)^guard((a_int32 ==_int32 0));
        (508.c:29#2)^goto lbl2;
      }
    } with lbl2: {
      (508.c:29#2)^do {
        (508.c:29#2)^choose {
         -->
          (508.c:29#2)^guard(! (c_int32 ==_int32 0));
          (508.c:29#2)^choose {
           -->
            (508.c:29#2)^guard(! (d_int32 ==_int32 0));
            (508.c:29#2)^goto lbl1;
           -->
            (508.c:29#2)^guard((d_int32 ==_int32 0));
            (508.c:29#2)^goto lbl3;
          }
         -->
          (508.c:29#2)^guard((c_int32 ==_int32 0));
          (508.c:29#2)^goto lbl3;
        }
      } with lbl3: {
        (508.c:35#4)^x =(int32) -2;
        (508.c:36#4)^x =(int32) -1;
        (508.c:37#4)^x =(int32) 1;
        (508.c:38#4)^x =(int32) 2;
      }
    }
  } with lbl1: {
    (508.c:30#4)^x =(int32) -2;
    (508.c:31#4)^x =(int32) -1;
    (508.c:32#4)^x =(int32) 0;
    (508.c:33#4)^x =(int32) 3;
  }
}

int32 a = 0;
int32 b = 0;
int32 c = 0;
int32 d = 0;
int32 x = 0;

