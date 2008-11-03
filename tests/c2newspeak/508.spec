Newspeak output
---------------
508.c
main() {
  (508.c:29#2)^do {
    (508.c:29#2)^do {
      (508.c:29#2)^choose {
        | ! (a_int32 ==_int32 0) -->
          (508.c:29#2)^choose {
            | ! (b_int32 ==_int32 0) -->
              (508.c:29#2)^goto lbl4;
            | (b_int32 ==_int32 0) -->
              (508.c:29#2)^goto lbl6;
          }
        | (a_int32 ==_int32 0) -->
          (508.c:29#2)^goto lbl6;
      }
    } with lbl6: {
      (508.c:29#2)^do {
        (508.c:29#2)^choose {
          | ! (c_int32 ==_int32 0) -->
            (508.c:29#2)^choose {
              | ! (d_int32 ==_int32 0) -->
                (508.c:29#2)^goto lbl4;
              | (d_int32 ==_int32 0) -->
                (508.c:29#2)^goto lbl5;
            }
          | (c_int32 ==_int32 0) -->
            (508.c:29#2)^goto lbl5;
        }
      } with lbl5: {
        (508.c:35#4)^x =(int32) -2;
        (508.c:36#4)^x =(int32) -1;
        (508.c:37#4)^x =(int32) 1;
        (508.c:38#4)^x =(int32) 2;
      }
    }
  } with lbl4: {
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

