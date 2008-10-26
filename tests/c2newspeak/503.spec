Warning: conditional expression accepted in 503.c line 29
Newspeak output
---------------
503.c
main() {
  (503.c:29#1094)^do {
    (503.c:29#1094)^do {
      (503.c:29#1094)^choose {
        | ! (a_int32 ==_int32 0) -->
          (503.c:29#1094)^choose {
            | ! (b_int32 ==_int32 0) -->
              (503.c:29#1094)^choose {
                | ! (d_int32 ==_int32 0) -->
                  (503.c:29#1094)^goto lbl4;
                | (d_int32 ==_int32 0) -->
                  (503.c:29#1094)^goto lbl5;
              }
            | (b_int32 ==_int32 0) -->
              (503.c:29#1094)^choose {
                | ! (e_int32 ==_int32 0) -->
                  (503.c:29#1094)^goto lbl4;
                | (e_int32 ==_int32 0) -->
                  (503.c:29#1094)^goto lbl5;
              }
          }
        | (a_int32 ==_int32 0) -->
          (503.c:29#1094)^choose {
            | ! (c_int32 ==_int32 0) -->
              (503.c:29#1094)^choose {
                | ! (d_int32 ==_int32 0) -->
                  (503.c:29#1094)^goto lbl4;
                | (d_int32 ==_int32 0) -->
                  (503.c:29#1094)^goto lbl5;
              }
            | (c_int32 ==_int32 0) -->
              (503.c:29#1094)^choose {
                | ! (e_int32 ==_int32 0) -->
                  (503.c:29#1094)^goto lbl4;
                | (e_int32 ==_int32 0) -->
                  (503.c:29#1094)^goto lbl5;
              }
          }
      }
    } with lbl4: {
      (503.c:30#1117)^x =(int32) 0;
      (503.c:31#1128)^x =(int32) 1;
      (503.c:32#1139)^x =(int32) 2;
      (503.c:33#1150)^x =(int32) 3;
      (503.c:34#1161)^x =(int32) 4;
    }
  } with lbl5: {
    (503.c:36#1183)^x =(int32) 0;
    (503.c:37#1194)^x =(int32) 1;
    (503.c:38#1205)^x =(int32) 2;
    (503.c:39#1216)^x =(int32) 3;
    (503.c:40#1227)^x =(int32) 4;
    (503.c:41#1238)^x =(int32) 5;
  }
}

int32 a = 0;
int32 b = 0;
int32 c = 0;
int32 d = 0;
int32 e = 0;
int32 x = 0;

