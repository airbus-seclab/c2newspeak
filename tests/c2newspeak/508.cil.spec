Newspeak output
---------------
508.c
main() {
  (508.c:29#1091)^choose {
    | ! (a_int32 ==_int32 0) -->
      (508.c:29#1091)^choose {
        | ! (b_int32 ==_int32 0) -->
          (508.c:30#1117)^x =(int32) -2;
          (508.c:31#1129)^x =(int32) -1;
          (508.c:32#1141)^x =(int32) 0;
          (508.c:33#1152)^x =(int32) 3;
        | (b_int32 ==_int32 0) -->
          (508.c:29#1091)^choose {
            | ! (c_int32 ==_int32 0) -->
              (508.c:29#1091)^choose {
                | ! (d_int32 ==_int32 0) -->
                  (508.c:30#1117)^x =(int32) -2;
                  (508.c:31#1129)^x =(int32) -1;
                  (508.c:32#1141)^x =(int32) 0;
                  (508.c:33#1152)^x =(int32) 3;
                | (d_int32 ==_int32 0) -->
                  (508.c:35#1174)^x =(int32) -2;
                  (508.c:36#1186)^x =(int32) -1;
                  (508.c:37#1198)^x =(int32) 1;
                  (508.c:38#1209)^x =(int32) 2;
              }
            | (c_int32 ==_int32 0) -->
              (508.c:35#1174)^x =(int32) -2;
              (508.c:36#1186)^x =(int32) -1;
              (508.c:37#1198)^x =(int32) 1;
              (508.c:38#1209)^x =(int32) 2;
          }
      }
    | (a_int32 ==_int32 0) -->
      (508.c:29#1091)^choose {
        | ! (c_int32 ==_int32 0) -->
          (508.c:29#1091)^choose {
            | ! (d_int32 ==_int32 0) -->
              (508.c:30#1117)^x =(int32) -2;
              (508.c:31#1129)^x =(int32) -1;
              (508.c:32#1141)^x =(int32) 0;
              (508.c:33#1152)^x =(int32) 3;
            | (d_int32 ==_int32 0) -->
              (508.c:35#1174)^x =(int32) -2;
              (508.c:36#1186)^x =(int32) -1;
              (508.c:37#1198)^x =(int32) 1;
              (508.c:38#1209)^x =(int32) 2;
          }
        | (c_int32 ==_int32 0) -->
          (508.c:35#1174)^x =(int32) -2;
          (508.c:36#1186)^x =(int32) -1;
          (508.c:37#1198)^x =(int32) 1;
          (508.c:38#1209)^x =(int32) 2;
      }
  }
}

int32 a = 0;
int32 b = 0;
int32 c = 0;
int32 d = 0;
int32 x = 0;

